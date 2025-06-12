(* Discretization logic for continuous dice *)

open Ast
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
  (* Helper function for comparison operations *)
  let handle_comparison aux op_name te1 te2 cmp_op flipped =
    let t1 = fst te1 in
    let t2 = fst te2 in
    let b1 = (match Ast.force t1 with
      | Ast.TFloat (b, _) -> b 
      | _ -> failwith ("Type error: " ^ op_name ^ " expects float on left operand")) 
    in
    let b2 = (match Ast.force t2 with
      | Ast.TFloat (b, _) -> b
      | _ -> failwith ("Type error: " ^ op_name ^ " expects float on right operand"))
    in
    let val1 = Bags.BoundBag.get b1 in
    let val2 = Bags.BoundBag.get b2 in
    if not (Bags.BoundSetContents.equal val1 val2) then 
      failwith ("Internal error: " ^ op_name ^ " operands have different bound bag values despite elaboration");

    (match val1 with 
      | Bags.Top -> 
          ExprNode (Cmp (cmp_op, aux te1, aux te2, flipped))
      | Bags.Finite bound_set -> 
          let n = 1 + List.length (Bags.BoundSet.elements bound_set) in
          let d1 = aux te1 in
          let d2 = aux te2 in 
          ExprNode (FinCmp (cmp_op, d1, d2, n, flipped))
    )
  in

  let rec aux ((ty, TAExprNode ae_node) : texpr) : expr =
    match ae_node with
    | Const f -> 
        let bounds_bag_ref = (match Ast.force ty with
          | Ast.TFloat (b, _) -> b (* Extract bounds bag *)
          | _ -> failwith "Type error: Const expects float") in
        (match Bags.BoundBag.get bounds_bag_ref with
         | Bags.Top -> ExprNode (Const f) (* Keep original if Top *)
         | Bags.Finite bound_set_from_context ->
            let idx, modulus = get_const_idx_and_modulus f bound_set_from_context in
              ExprNode (FinConst (idx, modulus))
        )

    | BoolConst b -> ExprNode (BoolConst b)

    | Var x ->
        ExprNode (Var x)

    | Let (x, te1, te2) ->
        ExprNode (Let (x, aux te1, aux te2))

    | Sample dist_exp ->
        let outer_sample_ty = ty in (* Type of the Sample expression itself *)
        let bounds_bag_of_outer_sample =
          match Ast.force outer_sample_ty with
          | Ast.TFloat (b, _) -> b
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
        | Bags.Finite outer_bound_set -> 
          (* Outer Sample's bounds are Finite, proceed with interval-based discretization *)
          let outer_cuts_as_bounds = Bags.BoundSet.elements outer_bound_set in
          let overall_modulus = 1 + List.length outer_cuts_as_bounds in 
          if overall_modulus <= 0 then failwith "Internal error: Sample modulus must be positive";

          let final_expr_producer (concrete_distr : Distributions.cdistr) : expr =
            let get_float_val_from_bound (b: Bags.bound) : float = 
              match b with Bags.Less f -> f | Bags.LessEq f -> f
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
            (* Allow for small floating point inaccuracies in probability sum;
               individual probabilities are clamped to [0,1] later. *)
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
            match Ast.force param_ty with
            | Ast.TFloat (_, consts_bag_ref) ->
              (match Bags.FloatBag.get consts_bag_ref with
               | Bags.Finite float_set -> 
                 if Bags.FloatSet.is_empty float_set then None 
                 else Some (Bags.FloatSet.elements float_set |> List.sort_uniq compare)
               | Bags.Top -> None)
            | _ -> None
          in

          let get_param_modulus_and_cuts (param_texpr : texpr) : (int * Bags.bound list) option =
            let param_ty, _ = param_texpr in
            match Ast.force param_ty with
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

          (* Helper function for 1-parameter distributions *)
          let handle_single_param_distribution kind param_texpr param_name =
            generate_runtime_match_for_param param_texpr param_name
              (fun param_value ->
                match Distributions.get_cdistr_from_single_arg_kind kind param_value with
                | Ok dist -> final_expr_producer dist
                | Error msg -> ExprNode (RuntimeError msg)
              )
              default_branch_expr
          in

          (* Helper function for 2-parameter distributions *)
          let handle_two_param_distribution kind param1_texpr param1_name param2_texpr param2_name hoist_suffix =
            let discretized_param2_expr = aux param2_texpr in
            let core_logic (eff_discretized_param2_expr : expr) =
              generate_runtime_match_for_param param1_texpr param1_name
                (fun param1_value ->
                  generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_param2_expr) param2_texpr param2_name
                    (fun param2_value ->
                      match Distributions.get_cdistr_from_two_arg_kind kind param1_value param2_value with
                      | Ok dist -> final_expr_producer dist
                      | Error msg -> ExprNode (RuntimeError msg)
                    )
                    default_branch_expr
                )
                default_branch_expr
            in
            (match discretized_param2_expr with
             | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                 core_logic discretized_param2_expr
             | _ ->
                 Util.gen_let ("_h_" ^ hoist_suffix) discretized_param2_expr (fun hoisted_var ->
                   core_logic (ExprNode (Var hoisted_var))
                 ))
          in

          match dist_exp with
          | Distr1 (DExponential, texpr_lambda) ->
              handle_single_param_distribution DExponential texpr_lambda "lambda"
          | Distr1 (DLaplace, texpr_scale) ->
              handle_single_param_distribution DLaplace texpr_scale "scale"
          | Distr1 (DCauchy, texpr_scale) ->
              handle_single_param_distribution DCauchy texpr_scale "scale"
          | Distr1 (DTDist, texpr_nu) ->
              handle_single_param_distribution DTDist texpr_nu "nu"
          | Distr1 (DChi2, texpr_nu) ->
              handle_single_param_distribution DChi2 texpr_nu "nu"
          | Distr1 (DLogistic, texpr_scale) ->
              handle_single_param_distribution DLogistic texpr_scale "scale"
          | Distr1 (DRayleigh, texpr_sigma) ->
              handle_single_param_distribution DRayleigh texpr_sigma "sigma"
          | Distr1 (DPoisson, texpr_mu) ->
              handle_single_param_distribution DPoisson texpr_mu "mu"
          
          | Distr2 (DUniform, texpr_a, texpr_b) ->
              handle_two_param_distribution DUniform texpr_a "a" texpr_b "b" "b"
          | Distr2 (DGaussian, texpr_mu, texpr_sigma) ->
              handle_two_param_distribution DGaussian texpr_mu "mu" texpr_sigma "sigma" "sigma"
          | Distr2 (DBeta, texpr_alpha, texpr_beta_param) ->
              handle_two_param_distribution DBeta texpr_alpha "alpha" texpr_beta_param "beta_param" "beta_p"
          | Distr2 (DLogNormal, texpr_mu, texpr_sigma) ->
              handle_two_param_distribution DLogNormal texpr_mu "mu" texpr_sigma "sigma" "sigma_ln"
          | Distr2 (DGamma, texpr_shape, texpr_scale) ->
              handle_two_param_distribution DGamma texpr_shape "shape" texpr_scale "scale" "scale_g"
          | Distr2 (DPareto, texpr_a, texpr_b) ->
              handle_two_param_distribution DPareto texpr_a "a" texpr_b "b" "b_p"
          | Distr2 (DWeibull, texpr_a, texpr_b) ->
              handle_two_param_distribution DWeibull texpr_a "a" texpr_b "b" "b_w"
          | Distr2 (DGumbel1, texpr_a, texpr_b) ->
              handle_two_param_distribution DGumbel1 texpr_a "a" texpr_b "b" "b_g1"
          | Distr2 (DGumbel2, texpr_a, texpr_b) ->
              handle_two_param_distribution DGumbel2 texpr_a "a" texpr_b "b" "b_g2"
          | Distr2 (DExppow, texpr_a, texpr_b) ->
              handle_two_param_distribution DExppow texpr_a "a" texpr_b "b" "b_ep"
          | Distr2 (DBinomial, texpr_p, texpr_n) ->
              handle_two_param_distribution DBinomial texpr_p "p" texpr_n "n" "b_bi"
          
        )
        
    | DistrCase cases ->
      (* Recursively discretize the expressions within the cases *) 
      let discretized_cases = 
        List.map (fun (texpr, prob) -> (aux texpr, prob)) cases 
      in
      ExprNode (DistrCase discretized_cases)

    | Cmp (cmp_op, te1, te2, flipped) ->
        let op_name = match cmp_op with
          | Ast.Lt -> "Less"
          | Ast.Le -> "LessEq" 
        in
        handle_comparison aux op_name te1 te2 cmp_op flipped

    | FinCmp (cmp_op, te1, te2, n, flipped) -> 
        ExprNode (FinCmp (cmp_op, aux te1, aux te2, n, flipped))

    | FinEq (te1, te2, n) ->
        ExprNode (FinEq (aux te1, aux te2, n))

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
  (* First set the bound bags to top in the top-level return type *)
  let (return_type, _) = e in
  Ast.set_bound_bags_to_top return_type;
  (* Then discretize *)
  discretize e