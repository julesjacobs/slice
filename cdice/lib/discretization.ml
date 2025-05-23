(* 
This module handles the conversion of typed expressions (texpr) 
with continuous components (like float constants and continuous distributions) 
into discrete expressions (expr). The discretization process is guided by 
bounds inferred during type checking, which are stored in 'bags' associated 
with float types. These bounds define the intervals for discretization.
*)

open Types
open Bags

(** Finds the first index of an element satisfying predicate [pred] in list [lst].
    Returns [Some index] if found, or [None] if no such element exists. *)
let find_index pred lst =
  let rec loop i = function
    | [] -> None
    | x::xs -> if pred x then Some i else loop (i+1) xs
  in
  loop 0 lst

(* ======== Helper for Constant Discretization ======== *)

(** Determines the discrete index and modulus for a float constant [f] based on a 
    given set of [bound_set_from_context] (cuts).
    - The [modulus] is defined as `1 + number of cuts`. This represents the total 
      number of discrete intervals created by the cuts.
    - The [idx] is the 0-based index of the interval where [f] falls. It's found 
      by identifying the first cut (bound) that [f] satisfies. If [f] satisfies 
      no cuts, it means [f] is in the last interval (index `modulus - 1`).
*)
let get_const_idx_and_modulus (f : float) (bound_set_from_context : BoundSet.t) : int * int =
  let cuts_as_bounds = BoundSet.elements bound_set_from_context in
  let modulus = 1 + List.length cuts_as_bounds in
  (* The index of the float is equal to the index of the first bound that is satisfied by f.
     For example, if cuts are [<1.0, <2.0, <3.0] (modulus 4):
     - f = 0.5 satisfies <1.0 (idx 0).
     - f = 1.5 satisfies <2.0 (idx 1 after <1.0).
     - f = 3.5 satisfies no bound, so it's in the last interval (idx 3). *)
  let idx = find_index (fun bound -> Bags.satisfies_bound f bound) cuts_as_bounds in
  let idx = match idx with
    | Some i -> i 
    | None -> modulus - 1 (* If no bound is satisfied, f is in the last interval. *)
  in
  (idx, modulus)

(** Calculates the probability mass of a continuous distribution [dist] 
    within the interval `[left, right)`. It uses the CDF of the distribution.
    Note: The interval is typically semi-open [left, right), but CDF usage
    effectively handles P(left <= X < right) or P(left < X <= right)
    depending on strictness of bounds and continuity of CDF. For continuous
    distributions, P(X=c) = 0, so strictness at endpoints often doesn't change mass.
*)
let prob_cdistr_interval (left : float) (right : float) (dist : Distributions.cdistr) : float =
  let cdf = Distributions.cdistr_cdf dist in
  cdf right -. cdf left

(** 
  [discretize e] converts a typed expression [e] (texpr) into a discrete 
  expression (expr). This process is the core of adapting continuous probabilistic 
  programs for discrete inference engines.
  
  Key aspects:
  - Floats: Float constants are mapped to discrete integer indices based on 
    inferred "bound bags" (sets of comparison points).
  - Continuous Distributions: Sample operations over continuous distributions are 
    transformed into discrete probability distributions (DistrCase) over the 
    intervals defined by these bounds.
  - Comparisons: Float comparisons (e.g., <, <=) are converted to finite 
    comparisons (FinCmp) that operate on these discrete indices.
  - Other constructs: Most other language constructs are recursively discretized.
*)
let discretize (e : texpr) : expr =
  (** [handle_comparison aux op_name te1 te2 cmp_op flipped] is a helper
      function to discretize comparison operations (like <, <=).
      It checks the bound bags of the operands [te1] and [te2].
      - If bound bags are [Top], it means no specific discretization context
        is available, so it generates a standard (non-finite) Cmp node.
      - If bound bags are [Finite bound_set], it means there's a set of
        cut points. The comparison is then transformed into a [FinCmp] node,
        which operates on discrete indices within the modulus defined by
        `1 + number of cuts`.
      It also ensures that both operands share the same bound bag information,
      which is expected after type elaboration.
  *)
  let handle_comparison aux op_name te1 te2 cmp_op flipped =
    let t1 = fst te1 in
    let t2 = fst te2 in
    let b1 = (match Types.force t1 with
      | Types.TFloat (b, _) -> b 
      | _ -> failwith ("Type error: " ^ op_name ^ " expects float on left operand")) 
    in
    let b2 = (match Types.force t2 with
      | Types.TFloat (b, _) -> b
      | _ -> failwith ("Type error: " ^ op_name ^ " expects float on right operand"))
    in
    let val1 = Bags.BoundBag.get b1 in
    let val2 = Bags.BoundBag.get b2 in
    if not (Bags.BoundSetContents.equal val1 val2) then 
      failwith ("Internal error: " ^ op_name ^ " operands have different bound bag values despite elaboration");

    (match val1 with 
      | Bags.Top -> 
          (* No specific bounds context, use general comparison *)
          ExprNode (Cmp (cmp_op, aux te1, aux te2, flipped))
      | Bags.Finite bound_set -> 
          (* Bounds context available, use finite domain comparison *)
          let n = 1 + List.length (Bags.BoundSet.elements bound_set) in (* Modulus *)
          let d1 = aux te1 in
          let d2 = aux te2 in 
          ExprNode (FinCmp (cmp_op, d1, d2, n, flipped))
    )
  in

  let rec aux ((ty, TAExprNode ae_node) : texpr) : expr =
    match ae_node with
    | Const f -> 
        let bounds_bag_ref = (match Types.force ty with
          | Types.TFloat (b, _) -> b (* Extract associated bounds bag from type *)
          | _ -> failwith "Type error: Const f expects float type for discretization context") in
        (match Bags.BoundBag.get bounds_bag_ref with
         | Bags.Top -> ExprNode (Const f) (* If context is Top, no discretization, keep as float const *)
         
         (* The following commented-out block seems like an alternative discretization strategy
            for floats when the bound set is empty. It might have been intended for a
            fixed-point representation or a default discretization scheme.
            Currently, if bound_set_from_context is empty, modulus is 1, idx is 0. *)
         (* | Bags.Finite bound_set when Bags.BoundSet.is_empty bound_set -> 
            let sz = (Util.bit_length (int_of_float f)) in if sz > !Util.curr_max_int_sz then Util.curr_max_int_sz := sz;
            ExprNode (FinConst (int_of_float f, 1)) *)
         | Bags.Finite bound_set_from_context ->
            (* Discretize the float constant based on the cuts in bound_set_from_context *)
            let idx, modulus = get_const_idx_and_modulus f bound_set_from_context in
              ExprNode (FinConst (idx, modulus))
        )

    | BoolConst b -> ExprNode (BoolConst b)

    | Var x -> ExprNode (Var x) (* Variables are passed through; their content will be discretized if needed when used *)

    | Let (x, te1, te2) -> ExprNode (Let (x, aux te1, aux te2))

    (* Discretization of a Sample expression. This is a core part of the process.
       Strategy:
       1. Examine the 'bounds_bag_of_outer_sample', which is the set of comparison points
          inferred for the *result* of this Sample operation.
       2. If this bag is `Bags.Top`, it means the sampled value is not directly compared
          or constrained by finite bounds in its context. Thus, the distribution itself
          is not discretized into intervals. We only recursively discretize its parameters.
       3. If this bag is `Bags.Finite outer_bound_set`, these bounds (cuts) are used to
          discretize the continuous distribution. The distribution is transformed into a
          discrete set of probabilities corresponding to the intervals defined by these cuts.
          Parameters to the distribution might themselves be expressions that need
          discretization or runtime evaluation (e.g. if a parameter is `Const 0.5` vs `Sample Uniform(0.0,1.0)`).
    *)
    | Sample dist_exp ->
        let outer_sample_ty = ty in
        let bounds_bag_of_outer_sample =
          match Types.force outer_sample_ty with
          | Types.TFloat (b, _) -> b
          | _ -> failwith "Internal error: Sample expression's type is not TFloat during discretize"
        in
        let set_or_top_val = Bags.BoundBag.get bounds_bag_of_outer_sample in
        
          (match set_or_top_val with
        | Bags.Top -> 
          (* Case 1: Outer Sample's bounds are Top.
             No interval-based discretization for the distribution itself.
             Fallback to simple recursive discretization of the distribution's parameters.
             The result will be a Sample node with (potentially) discretized parameters. *)
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
          (* Case 2: Outer Sample's bounds are Finite.
             Proceed with interval-based discretization of the continuous distribution.
             The `outer_cuts_as_bounds` define the intervals.
             `overall_modulus` is the number of discrete outcomes. *)
          let outer_cuts_as_bounds = Bags.BoundSet.elements outer_bound_set in
          let overall_modulus = 1 + List.length outer_cuts_as_bounds in 
          if overall_modulus <= 0 then failwith "Internal error: Sample modulus must be positive for Finite bounds";

          (** [final_expr_producer concrete_distr] takes a fully specified continuous 
              distribution (all parameters are concrete floats) and generates a 
              [DistrCase] expression. It calculates the probability mass for each 
              interval defined by `outer_cuts_as_bounds` and `overall_modulus`.
          *)
          let final_expr_producer (concrete_distr : Distributions.cdistr) : expr =
            let get_float_val_from_bound (b: Bags.bound) : float = 
              match b with Bags.Less f -> f | Bags.LessEq f -> f | Bags.Greater f -> f | Bags.GreaterEq f -> f
            in
            (* Create pairs of (left_bound, right_bound) for each interval *)
            let intervals_for_probs = List.init overall_modulus (fun k_idx ->
              (* k_idx is the discrete outcome index, from 0 to overall_modulus - 1 *)
              let left_for_cdf = 
                if k_idx = 0 then neg_infinity (* First interval starts at -infinity *)
                else get_float_val_from_bound (List.nth outer_cuts_as_bounds (k_idx - 1))
              in
              let right_for_cdf =
                if k_idx = overall_modulus - 1 then infinity (* Last interval ends at +infinity *)
                else get_float_val_from_bound (List.nth outer_cuts_as_bounds k_idx)
              in
              (* Ensure left <= right. If bounds imply same float val (e.g. LessEq 1.0, Less 1.0),
                 interval is [v,v], probability should be 0 by CDF difference. *)
              (min left_for_cdf right_for_cdf, max left_for_cdf right_for_cdf)
            ) in
            (* Calculate probability for each interval *)
            let probs = List.map (fun (l,r) -> prob_cdistr_interval l r concrete_distr) intervals_for_probs in
            (* Validate probabilities *)
            if List.exists (fun p -> p < -0.0001 || p > 1.0001) probs then
                failwith ("Internal error: generated probabilities are invalid (outside [0,1] range): " ^ Pretty.string_of_float_list probs ^ " for distribution " ^ Pretty.string_of_cdistr concrete_distr ^ Printf.sprintf " with %d outer bounds." (List.length outer_cuts_as_bounds));
            let sum_probs = List.fold_left (+.) 0.0 probs in
            if abs_float (sum_probs -. 1.0) > 0.001 then
               failwith ("Internal error: probabilities in discretized sample do not sum to 1.0 (sum=" ^ string_of_float sum_probs ^ "): " ^ Pretty.string_of_float_list probs ^ " for " ^ Pretty.string_of_cdistr concrete_distr);
            
            (* Create (FinConst, probability) pairs for DistrCase *)
            let distr_cases = List.mapi (fun i prob -> (ExprNode (FinConst (i, overall_modulus)), max 0.0 (min 1.0 prob) )) probs in
            ExprNode (DistrCase distr_cases)
          in

          (* Default expression if parameters to distribution cannot be resolved to constants for `final_expr_producer`.
             This typically means recursively discretizing the parameters but keeping the Sample structure. *)
          let default_branch_expr = 
            match dist_exp with
            | Distr1 (kind, texpr_arg) -> ExprNode (Sample (Distr1 (kind, aux texpr_arg)))
            | Distr2 (kind, texpr_arg1, texpr_arg2) -> ExprNode (Sample (Distr2 (kind, aux texpr_arg1, aux texpr_arg2)))
          in

          (** [get_possible_floats_from_param param_texpr] attempts to extract a sorted list
              of unique float constants if the parameter [param_texpr] has a `consts_bag_ref`
              (FloatBag) containing a finite set of known float values. Returns [None] if
              the bag is Top, empty, or not a float type. Used to check if a parameter
              is one of several known constants at runtime.
          *)
          let get_possible_floats_from_param (param_texpr : texpr) : float list option =
            let param_ty, _ = param_texpr in
            match Types.force param_ty with
            | Types.TFloat (_, consts_bag_ref) -> (* This is the FloatBag, not the BoundBag *)
              (match Bags.FloatBag.get consts_bag_ref with
               | Bags.Finite float_set -> 
                 if Bags.FloatSet.is_empty float_set then None 
                 else Some (Bags.FloatSet.elements float_set |> List.sort_uniq compare)
               | Bags.Top -> None)
            | _ -> None (* Parameter is not a float type *)
          in

          (** [get_param_modulus_and_cuts param_texpr] retrieves the modulus and the list of
              bound cuts associated with a TFloat parameter [param_texpr]. This information
              comes from the parameter's own `bounds_bag_ref` (BoundBag), which defines how
              this parameter itself would be discretized if it were a standalone constant.
              Returns [None] if the bag is Top, not TFloat, or modulus is invalid.
          *)
          let get_param_modulus_and_cuts (param_texpr : texpr) : (int * Bags.bound list) option =
            let param_ty, _ = param_texpr in
            match Types.force param_ty with
            | TFloat (b_bag, _) -> (* This is the BoundBag for the parameter *)
              (match Bags.BoundBag.get b_bag with
               | Top -> None 
               | Finite bs_set -> 
                 let cuts = Bags.BoundSet.elements bs_set in 
                 let modulus = 1 + List.length cuts in
                 if modulus <= 0 then None else Some (modulus, cuts))
            | _ -> None (* Parameter is not a float type *)
          in
          
          (** [build_nested_ifs val_var_name param_modulus arms default_expr_if_all_fail]
              Constructs a sequence of nested IF expressions (effectively a switch/match).
              - [val_var_name]: String name of the variable holding the discretized value to test.
              - [param_modulus]: The modulus for the FinEq comparison.
              - [arms]: A list of (target_finconst_expr, body_expr) pairs.
                `target_finconst_expr` is what `val_var_name` is compared against.
                `body_expr` is executed if the comparison is true.
              - [default_expr_if_all_fail]: Expression if no arm matches.
              Base cases:
              - No arms: Should ideally not be reached if caller ensures arms are non-empty,
                but has a failwith as a safeguard.
              - One arm: This becomes the 'else' branch of the preceding If, or the direct result
                if it's the only arm. The condition is implicitly true at this point.
              Recursive step: Creates an If node for the current arm's condition, with its body
                as the true branch, and recursively calls itself for the rest of the arms
                to form the false branch.
          *)
          let rec build_nested_ifs (val_var_name: string) (param_modulus: int) (arms: (expr * expr) list) (default_expr_if_all_fail: expr) : expr =
            match arms with 
            | [] -> 
                (* This safeguard implies that if `possible_floats` was non-empty, `arms` should also be non-empty.
                   If `possible_floats` was empty, `generate_runtime_match_for_param` should return `default_expr_for_this_param` directly.
                   Thus, reaching here with an empty list might indicate an unexpected state. *)
                failwith "build_nested_ifs: Reached empty arms list. This path should be preempted by checks in `generate_runtime_match_for_param` if `possible_floats` was empty, or implies an issue if it was non-empty."
            | [(_target_finconst_expr, body_expr)] -> 
                (* Last arm: if all prior FinEq checks failed, this is the effective 'else'.
                   The assumption is that `val_var_name` (the discretized parameter) *must* match one of the
                   `target_finconst_expr`s if `possible_floats` was exhaustive for that parameter.
                   So, the FinEq check is implicitly true here.
                *)
                body_expr
            | (target_finconst_expr, body_expr) :: rest_arms ->
              let current_val_expr = ExprNode (Var val_var_name) in
              let condition =
                ExprNode(FinEq(current_val_expr, target_finconst_expr, param_modulus))
              in
              ExprNode (If (condition, body_expr, build_nested_ifs val_var_name param_modulus rest_arms default_expr_if_all_fail))
          in

          (** [generate_runtime_match_for_param param_texpr param_name_str build_body_fn default_expr]
              Generates code to handle a distribution parameter ([param_texpr]) that might be one of
              several known float constants at runtime.
              - If the parameter's `consts_bag_ref` (FloatBag) is `Top` or empty, or its `bounds_bag_ref` (BoundBag) is `Top`,
                it falls back to [default_expr_for_this_param].
              - If it's a single known float, calls `build_body_fn` with that float.
              - If it's one of multiple known floats, it generates:
                `Let _disc_param = (aux param_texpr) In (If (FinEq _disc_param (aux Const_f1)) (build_body_fn f1) Else (If ...)))`
                This structure discretizes the parameter expression once, then uses nested Ifs
                (via `build_nested_ifs`) to compare its discretized value against the discretized
                values of the known possible floats. If a match is found, `build_body_fn` is called
                with that specific float to produce the final expression (often a `DistrCase` from `final_expr_producer`).
              - [already_discretized_expr]: Optionally provide the already discretized expression for the parameter if available.
          *)
          let generate_runtime_match_for_param 
              ?(already_discretized_expr : expr option = None)
              (param_texpr : texpr) 
              (param_name_str : string) 
              (build_body_fn : float -> expr) 
              (default_expr_for_this_param : expr) : expr =

            match get_possible_floats_from_param param_texpr, get_param_modulus_and_cuts param_texpr with
            | Some possible_floats, Some (param_modulus, param_actual_bound_cuts_as_bounds) ->
                if List.length possible_floats = 0 then default_expr_for_this_param (* No known floats, use default *)
                else if List.length possible_floats = 1 then 
                  build_body_fn (List.hd possible_floats) (* Only one possible float, directly build body *)
                else
                  (* Multiple possible floats: discretize the parameter and build nested Ifs *)
                  let actual_discretized_param_expr = 
                    match already_discretized_expr with
                    | Some ade -> ade (* Use provided if already discretized (e.g. for second param of two-param dist) *)
                    | None -> aux param_texpr (* Discretize the parameter expression *)
                  in
                  (* Create arms for build_nested_ifs: (target_discrete_const_expr, body_expr_for_this_const) *)
                  let match_arms = List.map (fun f_val ->
                    (* Create a temporary texpr for `Const f_val` with the parameter's own bound context
                       to correctly discretize this specific float value for comparison. *)
                    let param_consts_bag_for_f = Bags.FloatBag.create (Finite (FloatSet.singleton f_val)) in
                    let param_bounds_bag_for_f = Bags.BoundBag.create (Finite (Bags.BoundSet.of_list param_actual_bound_cuts_as_bounds)) in
                    let texpr_const_f_val = (TFloat(param_bounds_bag_for_f, param_consts_bag_for_f), TAExprNode (Const f_val)) in
                    let target_finconst_expr = aux texpr_const_f_val in (* This is the FinConst expr for f_val *)
                    let body = build_body_fn f_val in (* Body to execute if param is f_val *)
                    (target_finconst_expr, body)
                  ) possible_floats in
                  
                  (* Create a Let binding for the discretized parameter, then the If structure *)
                  Util.gen_let ("_disc_" ^ param_name_str) actual_discretized_param_expr 
                    (fun let_var_name -> build_nested_ifs let_var_name param_modulus match_arms default_expr_for_this_param)
            | _ -> default_expr_for_this_param (* Parameter is not a TFloat with finite consts/bounds, use default *)
          in

          (* [handle_single_param_distribution kind param_texpr param_name]
             Wrapper for distributions with one parameter. Uses `generate_runtime_match_for_param`
             to resolve the parameter to a float if possible, then tries to get the concrete
             distribution and produce the `DistrCase` via `final_expr_producer`.
             Falls back to `default_branch_expr` if param cannot be resolved.
          *)
          let handle_single_param_distribution kind param_texpr param_name =
            generate_runtime_match_for_param param_texpr param_name
              (fun param_value -> (* This is build_body_fn for generate_runtime_match_for_param *)
                match Distributions.get_cdistr_from_single_arg_kind kind param_value with
                | Ok dist -> final_expr_producer dist (* Successfully got a concrete distribution *)
                | Error msg -> ExprNode (RuntimeError msg) (* Invalid parameter value for distribution *)
              )
              default_branch_expr (* Fallback if param_value cannot be determined *)
          in

          (* [handle_two_param_distribution kind param1_texpr param1_name param2_texpr param2_name hoist_suffix]
             Wrapper for distributions with two parameters. It attempts to resolve both parameters.
             It first discretizes `param2_texpr`. If `param2_texpr` is complex, its discretized
             form is hoisted into a Let binding (`_h_...`) to avoid recomputation.
             Then, it calls `generate_runtime_match_for_param` for `param1_texpr`.
             Inside `param1`'s body function, it calls `generate_runtime_match_for_param` again
             for `param2_texpr` (passing the potentially hoisted, already discretized version).
             If both parameters are resolved to concrete floats, it gets the distribution
             via `Distributions.get_cdistr_from_two_arg_kind` and then uses `final_expr_producer`.
             Falls back to `default_branch_expr` at various stages if resolution fails.
          *)
          let handle_two_param_distribution kind param1_texpr param1_name param2_texpr param2_name hoist_suffix =
            let discretized_param2_expr = aux param2_texpr in
            let core_logic (eff_discretized_param2_expr : expr) = (* eff_discretized_param2_expr is param2 after potential hoisting *)
              generate_runtime_match_for_param param1_texpr param1_name
                (fun param1_value -> (* Body for param1 resolution *)
                  generate_runtime_match_for_param ~already_discretized_expr:(Some eff_discretized_param2_expr) param2_texpr param2_name
                    (fun param2_value -> (* Body for param2 resolution *)
                      match Distributions.get_cdistr_from_two_arg_kind kind param1_value param2_value with
                      | Ok dist -> final_expr_producer dist
                      | Error msg -> ExprNode (RuntimeError msg)
                    )
                    default_branch_expr (* Fallback if param2 cannot be resolved *)
                )
                default_branch_expr (* Fallback if param1 cannot be resolved *)
            in
            (* Hoist param2 if it's not a simple expression to avoid re-discretizing/re-evaluating it *)
            (match discretized_param2_expr with
             | ExprNode (Var _) | ExprNode (Const _) | ExprNode (BoolConst _) | ExprNode (FinConst _) ->
                 core_logic discretized_param2_expr (* Param2 is simple, no need to hoist *)
             | _ ->
                 (* Param2 is complex, hoist it into a let binding *)
                 Util.gen_let ("_h_" ^ hoist_suffix) discretized_param2_expr (fun hoisted_var ->
                   core_logic (ExprNode (Var hoisted_var))
                 ))
          in

          (* Dispatch based on the specific continuous distribution type *)
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
        )
        
    | DistrCase cases ->
      (* Discretize expressions within each branch of a DistrCase.
         Probabilities remain unchanged. *)
      let discretized_cases = 
        List.map (fun (texpr, prob) -> (aux texpr, prob)) cases 
      in
      ExprNode (DistrCase discretized_cases)

    | Cmp (cmp_op, te1, te2, flipped) ->
        (* Handle comparison using the helper function *)
        let op_name = match cmp_op with
          | Types.Lt -> "Less"
          | Types.Le -> "LessEq" 
        in
        handle_comparison aux op_name te1 te2 cmp_op flipped

    | FinCmp (cmp_op, te1, te2, n, flipped) -> 
        (* FinCmp already operates on discrete values, so just discretize subexpressions.
           The modulus 'n' is preserved. *)
        ExprNode (FinCmp (cmp_op, aux te1, aux te2, n, flipped))

    | FinEq (te1, te2, n) ->
        (* Similar to FinCmp, discretize subexpressions and preserve modulus. *)
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
        ExprNode (Fun (x, aux te)) (* Body of function is discretized *)
        
    | FuncApp (te1, te2) ->
        ExprNode (FuncApp (aux te1, aux te2))

    | LoopApp (te1, te2, te3) ->
        (* te3 (the integer count) is not a texpr, so not passed to aux *)
        ExprNode (LoopApp (aux te1, aux te2, te3))

    | FinConst (k, n) -> 
        (* Already a discrete finite constant, no change needed. *)
        ExprNode (FinConst (k, n))

    | And (te1, te2) -> ExprNode (And (aux te1, aux te2))
    | Or (te1, te2) -> ExprNode (Or (aux te1, aux te2))
    | Not te1 -> ExprNode (Not (aux te1))
    | Observe te1 -> ExprNode (Observe (aux te1))
    | Fix (f, x, te_body) -> ExprNode (Fix (f, x, aux te_body))
    | Nil -> ExprNode Nil
    | Cons (te_hd, te_tl) -> ExprNode (Cons (aux te_hd, aux te_tl))
    | MatchList (te_match, te_nil, y, ys, te_cons) ->
        ExprNode (MatchList (aux te_match, aux te_nil, y, ys, aux te_cons))
    | Ref te1 -> ExprNode (Ref (aux te1))
    | Deref te1 -> ExprNode (Deref (aux te1))
    | Assign (te1, te2) -> ExprNode (Assign (aux te1, aux te2))
    | Seq (te1, te2) -> ExprNode (Seq (aux te1, aux te2))
    | Unit -> ExprNode Unit
    | RuntimeError s -> ExprNode (RuntimeError s)
  in
  aux e

(** [discretize_top e] is the main entry point for discretizing a top-level expression.
    The commented-out lines suggest an intention to first normalize the bound bags
    of the overall return type of [e] to `Top`. This could be to ensure that the
    top-level expression isn't constrained by locally inferred bounds if it's meant
    to represent a value from a fully continuous domain before this specific
    discretization pass. This step is currently not active.
*)
let discretize_top (e : texpr) : expr =
  (* TODO: First set the bound bags to top in the top-level return type.
     This step is intended to prevent the top-level expression's own inferred
     bounds from overly constraining its discretization if it's meant to be evaluated
     in a broader context. Currently, this specific normalization is commented out
     and thus not applied. Its full impact would need careful consideration within
     the type inference and elaboration pipeline. *)
  (* let (return_type, _) = e in
     Types.set_float_bound_bags_to_top return_type; *)
  (* Then discretize *)
  discretize e 