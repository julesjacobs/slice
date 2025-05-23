(* Discretization logic for continuous dice *)

open Types
open Bags

let find_index pred lst =
  let rec loop i = function
    | [] -> None
    | x::xs -> if pred x then Some i else loop (i+1) xs
  in
  loop 0 lst

(* ======== Helper for Constant Discretization ======== *)

let get_const_idx_and_modulus (f : float) (bound_set_from_context : BoundSet.t) : int * int =
  let cuts_as_bounds = BoundSet.elements bound_set_from_context in
  let modulus = 1 + List.length cuts_as_bounds in
  (* The index of the float is equal to the index of the first bound that is satisfied *)
  let idx = find_index (fun bound -> Bags.satisfies_bound f bound) cuts_as_bounds in
  let idx = match idx with
    | Some i -> i 
    | None -> modulus - 1 (* If no bound is satisfied, we're in the last interval *)
  in
  (idx, modulus)

(* Calculate probability for a given distribution in an interval *)
let prob_cdistr_interval (left : float) (right : float) (dist : Distributions.cdistr) : float =
  let cdf = Distributions.cdistr_cdf dist in
  cdf right -. cdf left

(* 
Discretizer from typed expressions to discrete expressions.

The idea is that the type system infers a bag of floats that each 
expression possibly compares against. Instead of sampling from a continuous 
distribution, we sample from a discrete distribution that tells us the 
probabilities of the interval between two floats.

When doing a comparison against a float, we convert that to a comparison 
against the discrete integer that represents the i-th float in the bag.
*)
let discretize (e : texpr) : expr =
  let rec aux ((ty, TAExprNode ae_node) : texpr) : expr =
    match ae_node with
    | Const f -> 
        let bounds_bag_ref = (match Types.force ty with
          | Types.TFloat (b, _) -> b (* Extract bounds bag *)
          | _ -> failwith "Type error: Const expects float") in
        (match Bags.BoundBag.get bounds_bag_ref with
         | Bags.Top -> ExprNode (Const f) (* Keep original if Top *)
         (* For later : *)
         (* | Bags.Finite bound_set when Bags.BoundSet.is_empty bound_set -> 
            let sz = (Util.bit_length (int_of_float f)) in if sz > !Util.curr_max_int_sz then Util.curr_max_int_sz := sz;
            ExprNode (FinConst (int_of_float f, 1)) *)
         | Bags.Finite bound_set_from_context ->
            let idx, modulus = get_const_idx_and_modulus f bound_set_from_context in
              ExprNode (FinConst (idx, modulus)))

    | BoolConst b -> ExprNode (BoolConst b)

    | Var x ->
        ExprNode (Var x)

    | Let (x, te1, te2) ->
        ExprNode (Let (x, aux te1, aux te2))

    | Sample dist_exp ->
        let outer_sample_ty = ty in (* Type of the Sample expression itself *)
        let bounds_bag_of_outer_sample =
          match Types.force outer_sample_ty with
          | Types.TFloat (b, _) -> b
          | _ -> failwith "Internal error: Sample expression's type is not TFloat during discretize"
        in
        let set_or_top_val = Bags.BoundBag.get bounds_bag_of_outer_sample in
        
          (match set_or_top_val with
        | Bags.Top -> 
          (* If outer Sample's bounds are Top - no discretization for distribution, fallback to simple recursive discretization of params *)
          (match dist_exp with 
          | Distr1 (kind, texpr_arg) -> 
            let texpr_arg_discretized = aux texpr_arg in
            ExprNode (Sample (Distr1 (kind, texpr_arg_discretized)))
          | Distr2 (kind, texpr_arg1, texpr_arg2) -> 
            let texpr_arg1_discretized = aux texpr_arg1 in
            let texpr_arg2_discretized = aux texpr_arg2 in
            ExprNode (Sample (Distr2 (kind, texpr_arg1_discretized, texpr_arg2_discretized)))
          )
        (* For later : *)
        (* | Bags.Finite outer_bound_set when Bags.BoundSet.is_empty outer_bound_set ->
            (* Empty bound set - no discretization for distribution, fallback to simple recursive discretization of params *)
            (match dist_exp with
            | Distr1 (kind, texpr_arg) -> 
                let texpr_arg_discretized = aux texpr_arg in
                ExprNode (Sample (Distr1 (kind, texpr_arg_discretized)))
            | Distr2 (kind, texpr_arg1, texpr_arg2) -> 
                let texpr_arg1_discretized = aux texpr_arg1 in
                let texpr_arg2_discretized = aux texpr_arg2 in
                ExprNode (Sample (Distr2 (kind, texpr_arg1_discretized, texpr_arg2_discretized)))
            ) *)
        | Bags.Finite outer_bound_set -> 
          (* Outer Sample's bounds are Finite, proceed with interval-based discretization *)
          let outer_cuts_as_bounds = Bags.BoundSet.elements outer_bound_set in
          let overall_modulus = 1 + List.length outer_cuts_as_bounds in 
          if overall_modulus <= 0 then failwith "Internal error: Sample modulus must be positive";

          let final_expr_producer (concrete_distr : Distributions.cdistr) : expr =
            let get_float_val_from_bound (b: Bags.bound) : float = 
              match b with Bags.Less f -> f | Bags.LessEq f -> f | Bags.Greater f -> f | Bags.GreaterEq f -> f
            in

            let intervals_for_probs = List.init overall_modulus (fun k_idx ->
              (* k_idx is the discrete outcome index, from 0 to overall_modulus - 1 *)
              let left_for_cdf = 
                if k_idx = 0 then neg_infinity
                else get_float_val_from_bound (List.nth outer_cuts_as_bounds (k_idx - 1))
              in
              let right_for_cdf =
                if k_idx = overall_modulus - 1 then infinity
                else get_float_val_from_bound (List.nth outer_cuts_as_bounds k_idx)
              in
              (* Ensure left <= right. If bounds imply same float val, interval is [v,v] -> prob should be 0 by CDF diff *)
              (min left_for_cdf right_for_cdf, max left_for_cdf right_for_cdf)
            ) in
            let probs = List.map (fun (l,r) -> prob_cdistr_interval l r concrete_distr) intervals_for_probs in
            if List.exists (fun p -> p < -0.0001 || p > 1.0001) probs then
                failwith ("Internal error: generated probabilities are invalid: " ^ Pretty.string_of_float_list probs ^ " for distribution " ^ Pretty.string_of_cdistr concrete_distr ^ Printf.sprintf " with %d outer bounds." (List.length outer_cuts_as_bounds));
            let sum_probs = List.fold_left (+.) 0.0 probs in
            if abs_float (sum_probs -. 1.0) > 0.001 then
               (); 
            
            let distr_cases = List.mapi (fun i prob -> (ExprNode (FinConst (i, overall_modulus)), max 0.0 (min 1.0 prob) )) probs in
            ExprNode (DistrCase distr_cases)
          in

          let default_branch_expr = 
            match dist_exp with
            | Distr1 (kind, texpr_arg) -> ExprNode (Sample (Distr1 (kind, aux texpr_arg)))
            | Distr2 (kind, texpr_arg1, texpr_arg2) -> ExprNode (Sample (Distr2 (kind, aux texpr_arg1, aux texpr_arg2)))
          in

          let get_possible_floats_from_param (param_texpr : texpr) : float list option =
            let param_ty, _ = param_texpr in
            match Types.force param_ty with
            | Types.TFloat (_, consts_bag_ref) ->
              (match Bags.FloatBag.get consts_bag_ref with
               | Bags.Finite float_set -> 
                 if Bags.FloatSet.is_empty float_set then None 
                 else Some (Bags.FloatSet.elements float_set |> List.sort_uniq compare)
               | Bags.Top -> None)
            | _ -> None
          in

          let get_param_modulus_and_cuts (param_texpr : texpr) : (int * Bags.bound list) option =
            let param_ty, _ = param_texpr in
            match Types.force param_ty with
            | TFloat (b_bag, _) -> 
              (match Bags.BoundBag.get b_bag with
               | Top -> None 
               | Finite bs_set -> 
                 let cuts = Bags.BoundSet.elements bs_set in 
                 let modulus = 1 + List.length cuts in
                 if modulus <= 0 then None else Some (modulus, cuts))
            | _ -> None
          in
          
          let rec build_nested_ifs (val_var_name: string) (param_modulus: int) (arms: (expr * expr) list) (default_expr_if_all_fail: expr) : expr =
            match arms with 
            | [] -> 
                (* This case should ideally not be reached if `generate_runtime_match_for_param` 
                   ensures `arms` (derived from `possible_floats`) is non-empty when calling this.
                   If `possible_floats` is empty, `default_expr_if_all_fail` is returned directly by the caller.
                   If it *is* reached, it means something unexpected happened or initial arms list was empty. *) 
                failwith "build_nested_ifs: Reached empty arms list, this should be handled by caller or indicates an issue."
            | [(_target_finconst_expr, body_expr)] -> 
                (* This is the last arm. If all previous conditions were false, this one is effectively the 'else'.
                   The assumption is that val_var_name *must* match one of the targets if possible_floats was exhaustive.
                   So, the condition FinEq(ExprNode (Var val_var_name), target_finconst_expr, param_modulus) is implicitly true here.
                *)
                body_expr
            | (target_finconst_expr, body_expr) :: rest_arms ->
              let current_val_expr = ExprNode (Var val_var_name) in
              let condition =
                ExprNode(FinEq(current_val_expr, target_finconst_expr, param_modulus))
              in
              ExprNode (If (condition, body_expr, build_nested_ifs val_var_name param_modulus rest_arms default_expr_if_all_fail))
          in

          let generate_runtime_match_for_param 
              ?(already_discretized_expr : expr option = None)
              (param_texpr : texpr) 
              (param_name_str : string) 
              (build_body_fn : float -> expr) 
              (default_expr_for_this_param : expr) : expr =

            match get_possible_floats_from_param param_texpr, get_param_modulus_and_cuts param_texpr with
            | Some possible_floats, Some (param_modulus, param_actual_bound_cuts_as_bounds) ->
                if List.length possible_floats = 0 then default_expr_for_this_param
                else if List.length possible_floats = 1 then 
                  build_body_fn (List.hd possible_floats)
                else
                  let actual_discretized_param_expr = 
                    match already_discretized_expr with
                    | Some ade -> ade
                    | None -> aux param_texpr
                  in
                  let match_arms = List.map (fun f_val ->
                    let param_consts_bag_for_f = Bags.FloatBag.create (Finite (FloatSet.singleton f_val)) in
                    let param_bounds_bag_for_f = Bags.BoundBag.create (Finite (Bags.BoundSet.of_list param_actual_bound_cuts_as_bounds)) in
                    let texpr_const_f_val = (TFloat(param_bounds_bag_for_f, param_consts_bag_for_f), TAExprNode (Const f_val)) in
                    let target_finconst_expr = aux texpr_const_f_val in
                    let body = build_body_fn f_val in
                    (target_finconst_expr, body)
                  ) possible_floats in
                  
                  Util.gen_let ("_disc_" ^ param_name_str) actual_discretized_param_expr 
                    (fun let_var_name -> build_nested_ifs let_var_name param_modulus match_arms default_expr_for_this_param)
            | _ -> default_expr_for_this_param
          in

          match dist_exp with
          | Distr1 (DExponential, texpr_lambda) ->
              generate_runtime_match_for_param texpr_lambda "lambda"
                (fun val_lambda ->
                  if val_lambda <= 0.0 then
                    ExprNode (RuntimeError "Exponential lambda must be positive")
                  else
                    final_expr_producer (Distributions.Exponential val_lambda)
                )
                default_branch_expr
          | Distr1 (DLaplace, texpr_scale) ->
              generate_runtime_match_for_param texpr_scale "scale"
                (fun val_scale ->
                  if val_scale <= 0.0 then
                    ExprNode (RuntimeError "Laplace scale must be positive")
                  else
                    final_expr_producer (Distributions.Laplace val_scale)
                )
                default_branch_expr
          | Distr1 (DCauchy, texpr_scale) ->
              generate_runtime_match_for_param texpr_scale "scale"
                (fun val_scale ->
                  if val_scale <= 0.0 then
                    ExprNode (RuntimeError "Cauchy scale must be positive")
                  else
                    final_expr_producer (Distributions.Cauchy val_scale)
                )
                default_branch_expr
          | Distr1 (DTDist, texpr_nu) ->
              generate_runtime_match_for_param texpr_nu "nu"
                (fun val_nu ->
                  if val_nu <= 0.0 then
                    ExprNode (RuntimeError "TDist nu must be positive")
                  else
                    final_expr_producer (Distributions.TDist val_nu)
                )
                default_branch_expr
          | Distr1 (DChi2, texpr_nu) ->
              generate_runtime_match_for_param texpr_nu "nu"
                (fun val_nu ->
                  if val_nu <= 0.0 then
                    ExprNode (RuntimeError "Chi2 nu must be positive")
                  else
                    final_expr_producer (Distributions.Chi2 val_nu)
                )
                default_branch_expr
          | Distr1 (DLogistic, texpr_scale) ->
              generate_runtime_match_for_param texpr_scale "scale"
                (fun val_scale ->
                  if val_scale <= 0.0 then
                    ExprNode (RuntimeError "Logistic scale must be positive")
                  else
                    final_expr_producer (Distributions.Logistic val_scale)
                )
                default_branch_expr
          | Distr1 (DRayleigh, texpr_sigma) ->
              generate_runtime_match_for_param texpr_sigma "sigma"
                (fun val_sigma ->
                  if val_sigma <= 0.0 then
                    ExprNode (RuntimeError "Rayleigh sigma must be positive")
                  else
                    final_expr_producer (Distributions.Rayleigh val_sigma)
                )
                default_branch_expr
          
          | Distr2 (DUniform, texpr_a, texpr_b) ->
              let discretized_b_expr = aux texpr_b in
              let core_logic (eff_discretized_b_expr : expr) =
                generate_runtime_match_for_param texpr_a "a"
                  (fun val_a -> 
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_b_expr) texpr_b "b"
                      (fun val_b -> 
                        if val_a > val_b then
                          ExprNode (RuntimeError "Uniform low > high")
                        else
                          final_expr_producer (Distributions.Uniform (val_a, val_b))
                      )
                      default_branch_expr 
                  )
                  default_branch_expr
              in
              (match discretized_b_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_b_expr
               | _ ->
                   Util.gen_let "_h_b" discretized_b_expr (fun hoisted_b_var ->
                     core_logic (ExprNode (Var hoisted_b_var))
                   ))

          | Distr2 (DGaussian, texpr_mu, texpr_sigma) ->
              let discretized_sigma_expr = aux texpr_sigma in
              let core_logic (eff_discretized_sigma_expr : expr) =
                generate_runtime_match_for_param texpr_mu "mu"
                  (fun val_mu ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_sigma_expr) texpr_sigma "sigma"
                      (fun val_sigma ->
                        if val_sigma <= 0.0 then
                          ExprNode (RuntimeError "Gaussian sigma must be positive")
                        else
                          final_expr_producer (Distributions.Gaussian (val_mu, val_sigma))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_sigma_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_sigma_expr
               | _ ->
                   Util.gen_let "_h_sigma" discretized_sigma_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))

          | Distr2 (DBeta, texpr_alpha, texpr_beta_param) ->
              let discretized_beta_param_expr = aux texpr_beta_param in
              let core_logic (eff_discretized_beta_param_expr : expr) =
                generate_runtime_match_for_param texpr_alpha "alpha"
                  (fun val_alpha ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_beta_param_expr) texpr_beta_param "beta_param"
                      (fun val_beta_param ->
                        if val_alpha <= 0.0 || val_beta_param <= 0.0 then
                          ExprNode (RuntimeError "Beta alpha and beta_param must be positive")
                        else
                          final_expr_producer (Distributions.Beta (val_alpha, val_beta_param))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_beta_param_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_beta_param_expr
               | _ ->
                   Util.gen_let "_h_beta_p" discretized_beta_param_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))

          | Distr2 (DLogNormal, texpr_mu, texpr_sigma) ->
              let discretized_sigma_expr = aux texpr_sigma in
              let core_logic (eff_discretized_sigma_expr : expr) =
                generate_runtime_match_for_param texpr_mu "mu"
                  (fun val_mu ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_sigma_expr) texpr_sigma "sigma"
                      (fun val_sigma ->
                        if val_sigma <= 0.0 then
                          ExprNode (RuntimeError "LogNormal sigma must be positive")
                        else
                          final_expr_producer (Distributions.LogNormal (val_mu, val_sigma))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_sigma_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_sigma_expr
               | _ ->
                   Util.gen_let "_h_sigma_ln" discretized_sigma_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))

          | Distr2 (DGamma, texpr_shape, texpr_scale) ->
              let discretized_scale_expr = aux texpr_scale in
              let core_logic (eff_discretized_scale_expr : expr) =
                generate_runtime_match_for_param texpr_shape "shape"
                  (fun val_shape ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_scale_expr) texpr_scale "scale"
                      (fun val_scale ->
                        if val_shape <= 0.0 || val_scale <= 0.0 then
                          ExprNode (RuntimeError "Gamma shape and scale must be positive")
                        else
                          final_expr_producer (Distributions.Gamma (val_shape, val_scale))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_scale_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_scale_expr
               | _ ->
                   Util.gen_let "_h_scale_g" discretized_scale_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))

          | Distr2 (DPareto, texpr_a, texpr_b) ->
              let discretized_b_expr = aux texpr_b in
              let core_logic (eff_discretized_b_expr : expr) =
                generate_runtime_match_for_param texpr_a "a"
                  (fun val_a ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_b_expr) texpr_b "b"
                      (fun val_b ->
                        if val_a <= 0.0 || val_b <= 0.0 then
                          ExprNode (RuntimeError "Pareto a and b must be positive")
                        else
                          final_expr_producer (Distributions.Pareto (val_a, val_b))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_b_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_b_expr
               | _ ->
                   Util.gen_let "_h_b_p" discretized_b_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))

          | Distr2 (DWeibull, texpr_a, texpr_b) ->
              let discretized_b_expr = aux texpr_b in
              let core_logic (eff_discretized_b_expr : expr) =
                generate_runtime_match_for_param texpr_a "a"
                  (fun val_a ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_b_expr) texpr_b "b"
                      (fun val_b ->
                        if val_a <= 0.0 || val_b <= 0.0 then
                          ExprNode (RuntimeError "Weibull a and b must be positive")
                        else
                          final_expr_producer (Distributions.Weibull (val_a, val_b))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_b_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_b_expr
               | _ ->
                   Util.gen_let "_h_b_w" discretized_b_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))

          | Distr2 (DGumbel1, texpr_a, texpr_b) ->
              let discretized_b_expr = aux texpr_b in
              let core_logic (eff_discretized_b_expr : expr) =
                generate_runtime_match_for_param texpr_a "a"
                  (fun val_a ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_b_expr) texpr_b "b"
                      (fun val_b ->
                        if val_b <= 0.0 then (* Gumbel1 constraint is on b *)
                          ExprNode (RuntimeError "Gumbel1 b must be positive")
                        else
                          final_expr_producer (Distributions.Gumbel1 (val_a, val_b))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_b_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_b_expr
               | _ ->
                   Util.gen_let "_h_b_g1" discretized_b_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))

          | Distr2 (DGumbel2, texpr_a, texpr_b) ->
              let discretized_b_expr = aux texpr_b in
              let core_logic (eff_discretized_b_expr : expr) =
                generate_runtime_match_for_param texpr_a "a"
                  (fun val_a ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_b_expr) texpr_b "b"
                      (fun val_b ->
                        if val_b <= 0.0 then (* Gumbel2 constraint is on b *)
                          ExprNode (RuntimeError "Gumbel2 b must be positive")
                        else
                          final_expr_producer (Distributions.Gumbel2 (val_a, val_b))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_b_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_b_expr
               | _ ->
                   Util.gen_let "_h_b_g2" discretized_b_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))

          | Distr2 (DExppow, texpr_a, texpr_b) ->
              let discretized_b_expr = aux texpr_b in
              let core_logic (eff_discretized_b_expr : expr) =
                generate_runtime_match_for_param texpr_a "a"
                  (fun val_a ->
                    generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_b_expr) texpr_b "b"
                      (fun val_b ->
                        if val_a <= 0.0 || val_b <= 0.0 then
                          ExprNode (RuntimeError "Exppow a and b must be positive")
                        else
                          final_expr_producer (Distributions.Exppow (val_a, val_b))
                      )
                      default_branch_expr
                  )
                  default_branch_expr
              in
              (match discretized_b_expr with
               | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                   core_logic discretized_b_expr
               | _ ->
                   Util.gen_let "_h_b_ep" discretized_b_expr (fun hoisted_var ->
                     core_logic (ExprNode (Var hoisted_var))
                   ))
        )
        
    | DistrCase cases ->
      (* Recursively discretize the expressions within the cases *) 
      let discretized_cases = 
        List.map (fun (texpr, prob) -> (aux texpr, prob)) cases 
      in
      ExprNode (DistrCase discretized_cases)

    | Less (te1, te2) ->
        let t1 = fst te1 in
        let t2 = fst te2 in
        let b1 = (match Types.force t1 with
          | Types.TFloat (b, _) -> b 
          | _ -> failwith "Type error: Less expects float") 
        in
        let b2 = (match Types.force t2 with
          | Types.TFloat (b, _) -> b
          | _ -> failwith "Type error: Less expects float on right operand")
        in
        let val1 = Bags.BoundBag.get b1 in
        let val2 = Bags.BoundBag.get b2 in
        if not (Bags.BoundSetContents.equal val1 val2) then 
          failwith "Internal error: Less operands have different bound bag values despite elaboration";

        (match val1 with 
          | Bags.Top -> 
              ExprNode (Less (aux te1, aux te2)) 
          | Bags.Finite bound_set -> 
              let n = 1 + List.length (Bags.BoundSet.elements bound_set) in
              let d1 = aux te1 in
              let d2 = aux te2 in 
              ExprNode (FinLt (d1, d2, n)))
        
    | LessEq (te1, te2) -> 
        let t1 = fst te1 in
        let t2 = fst te2 in
        let b1 = (match Types.force t1 with
          | Types.TFloat (b, _) -> b 
          | _ -> failwith "Type error: LessEq expects float on left operand") 
        in
        let b2 = (match Types.force t2 with
          | Types.TFloat (b, _) -> b
          | _ -> failwith "Type error: LessEq expects float on right operand")
        in
        let val1 = Bags.BoundBag.get b1 in
        let val2 = Bags.BoundBag.get b2 in
        if not (Bags.BoundSetContents.equal val1 val2) then 
          failwith "Internal error: LessEq operands have different bound bag values despite elaboration";
        
        (match val1 with 
          | Bags.Top -> 
              ExprNode (LessEq (aux te1, aux te2))
          | Bags.Finite bound_set -> 
              let n = 1 + List.length (Bags.BoundSet.elements bound_set) in
              let d1 = aux te1 in
              let d2 = aux te2 in 
              ExprNode (FinLeq (d1, d2, n)))

    | Greater (te1, te2) ->
      let t1 = fst te1 in
      let t2 = fst te2 in
      let b1 = (match Types.force t1 with
        | Types.TFloat (b, _) -> b 
        | _ -> failwith "Type error: Greater expects float") 
      in
      let b2 = (match Types.force t2 with
        | Types.TFloat (b, _) -> b
        | _ -> failwith "Type error: Greater expects float on right operand")
      in
      let val1 = Bags.BoundBag.get b1 in
      let val2 = Bags.BoundBag.get b2 in
      if not (Bags.BoundSetContents.equal val1 val2) then 
        failwith "Internal error: Greater operands have different bound bag values despite elaboration";

      (match val1 with 
        | Bags.Top -> 
            ExprNode (Greater (aux te1, aux te2)) 
        | Bags.Finite bound_set -> 
            let n = 1 + List.length (Bags.BoundSet.elements bound_set) in
            let d1 = aux te1 in
            let d2 = aux te2 in 
            ExprNode (FinGt (d1, d2, n)))

    | GreaterEq (te1, te2) -> 
      let t1 = fst te1 in
      let t2 = fst te2 in
      let b1 = (match Types.force t1 with
        | Types.TFloat (b, _) -> b 
        | _ -> failwith "Type error: GreaterEq expects float on left operand") 
      in
      let b2 = (match Types.force t2 with
        | Types.TFloat (b, _) -> b
        | _ -> failwith "Type error: GreaterEq expects float on right operand")
      in
      let val1 = Bags.BoundBag.get b1 in
      let val2 = Bags.BoundBag.get b2 in
      if not (Bags.BoundSetContents.equal val1 val2) then 
        failwith "Internal error: GreaterEq operands have different bound bag values despite elaboration";
      
      (match val1 with 
        | Bags.Top -> 
            ExprNode (GreaterEq (aux te1, aux te2))
        | Bags.Finite bound_set -> 
            let n = 1 + List.length (Bags.BoundSet.elements bound_set) in
            let d1 = aux te1 in
            let d2 = aux te2 in 
            ExprNode (FinGeq (d1, d2, n)))

    | If (te1, te2, te3) ->
        ExprNode (If (aux te1, aux te2, aux te3))
        
    | Pair (te1, te2) ->
        ExprNode (Pair (aux te1, aux te2))
        
    | First te ->
        ExprNode (First (aux te))
        
    | Second te ->
        ExprNode (Second (aux te))
        
    | Fun (x, te) ->
        ExprNode (Fun (x, aux te))
        
    | FuncApp (te1, te2) ->
        ExprNode (FuncApp (aux te1, aux te2))

    | LoopApp (te1, te2, te3) ->
        ExprNode (LoopApp (aux te1, aux te2, te3))

    | FinConst (k, n) -> 
        ExprNode (FinConst (k, n))
    | FinLt (te1, te2, n) -> 
        ExprNode (FinLt (aux te1, aux te2, n))
    | FinLeq (te1, te2, n) -> 
        ExprNode (FinLeq (aux te1, aux te2, n))
    | FinGt (te1, te2, n) -> 
        ExprNode (FinGt (aux te1, aux te2, n))
    | FinGeq (te1, te2, n) -> 
        ExprNode (FinGeq (aux te1, aux te2, n))
    | FinEq (te1, te2, n) ->
        ExprNode (FinEq (aux te1, aux te2, n))

    | And (te1, te2) ->
        ExprNode (And (aux te1, aux te2))
        
    | Or (te1, te2) ->
        ExprNode (Or (aux te1, aux te2))
        
    | Not te1 ->
        ExprNode (Not (aux te1))

    | Observe te1 ->
        ExprNode (Observe (aux te1))

    | Fix (f, x, te_body) -> 
        ExprNode (Fix (f, x, aux te_body))

    | Nil -> ExprNode Nil

    | Cons (te_hd, te_tl) -> 
        ExprNode (Cons (aux te_hd, aux te_tl))

    | MatchList (te_match, te_nil, y, ys, te_cons) ->
        ExprNode (MatchList (aux te_match, aux te_nil, y, ys, aux te_cons))

    | Ref te1 -> 
        ExprNode (Ref (aux te1))
    | Deref te1 -> 
        ExprNode (Deref (aux te1))
    | Assign (te1, te2) -> 
        ExprNode (Assign (aux te1, aux te2))

    | Seq (te1, te2) ->
        ExprNode (Seq (aux te1, aux te2))

    | Unit -> ExprNode Unit

    | RuntimeError s -> ExprNode (RuntimeError s)

  in
  aux e

let discretize_top (e : texpr) : expr =
  (* TODO: First set the bound bags to top in the top-level return type *)
  (* let (return_type, _) = e in
     Types.set_float_bound_bags_to_top return_type; *)
  (* Then discretize *)
  discretize e 