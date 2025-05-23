(*
This module implements the type inference and elaboration system for the language.
It traverses an abstract syntax tree (AST) of type `expr`, performs type checking,
and generates a typed AST of type `texpr`. A key aspect of this process is the
management of type constraints, particularly for float types which involve "bags"
to collect information about bounds (BoundBag) and concrete constant values (FloatBag).
These bags are used to guide the discretization process for continuous constructs.
The system uses unification and subtyping on `ty` types, including meta variables
for type inference.
*)

open Types
open Bags

module StringMap = Map.Make(String)

(** [sub_type t_sub t_super] enforces that [t_sub] is a subtype of [t_super].
    It recursively checks structural types and handles meta variables.
    Type rules:
    - Base types (TBool, TFin n, TUnit) must match exactly.
    - TFloat(b1, c1) <: TFloat(b2, c2):
        BoundBags `b1` and `b2` must be equivalent (unified using `Bags.BoundBag.eq`).
        This means they must represent the same set of discretization points.
        ConstsBags `c1` can be a subtype of `c2` (`Bags.FloatBag.leq c1 c2`), meaning
        the set of concrete constants in `c1` is a subset of or equal to those in `c2`.
        This allows, for example, a function expecting a float that could be {1.0}
        to accept a float that is known to be {1.0, 2.0}, but not vice-versa if
        the function specifically needs to know about 2.0. (Correction: `leq c1 c2` means constants flow from sub to super, so `c1`'s constants must be a subset of `c2`'s *potential* constants, or if `c2` is Top. If `c1` is `Finite {1.0}` and `c2` is `Finite {1.0, 2.0}`, this is fine. If `c1` is `Finite {1.0, 2.0}` and `c2` is `Finite {1.0}`, this is not fine.)
        Actually, for `FloatBag.leq c1 c2`, if `c1` is `Finite S1` and `c2` is `Finite S2`, it means `S1` must be a subset of `S2`. If `c2` is `Top`, `c1` can be anything. This means a context requiring a more general set of floats (or Top) can accept a more specific one.
    - TPair: Covariant in both components. (A pair (X,Y) is a subtype of (A,B) if X<:A and Y<:B).
    - TFun: Contravariant in the argument type, covariant in the result type.
      (A function X->Y is a subtype of A->B if A<:X (argument) and Y<:B (result)).
    - TList: Covariant in the element type.
    - TRef: Invariant (must unify, meaning types must be equivalent).
    - TMeta: If one type is a meta variable, attempt to assign the other type to it.
      If both are meta variables, set up listeners so that if one is assigned,
      the subtyping relation is re-checked.
*)
let rec sub_type (t_sub : ty) (t_super : ty) : unit =
  match Types.force t_sub, Types.force t_super with
  (* Base Cases: Exact match required *)
  | TBool,    TBool      -> ()
  | TFin n1, TFin n2 when n1 = n2 -> () 
  | TUnit, TUnit -> ()
  (* Structural Cases *)
  | TFloat (b1, c1), TFloat (b2, c2) -> 
      Bags.BoundBag.eq b1 b2;  (* BoundBags must be equivalent (same discretization context) *) 
      Bags.FloatBag.leq c1 c2  (* ConstsBag subtyping: constants in t_sub must be compatible with t_super (e.g. c1 is subset of c2, or c2 is Top) *)
  | TPair(a1, b1), TPair(a2, b2) -> 
      sub_type a1 a2; (* Covariant in the first component *) 
      sub_type b1 b2  (* Covariant in the second component *) 
  | TFun(a1, b1), TFun(a2, b2) -> 
      sub_type a2 a1; (* Contravariant in the argument type *) 
      sub_type b1 b2  (* Covariant in the result type *) 
  | TList t1, TList t2 -> sub_type t1 t2 (* Covariant in element type *) 
  | TRef t1, TRef t2 -> unify t1 t2 (* References are invariant: types must be equivalent *)
  (* TMeta Handling:
     If t_sub is a meta variable `r`:
     - If t_super is also a meta `r'`, set up listeners to re-check subtyping when either `r` or `r'` is bound.
     - Otherwise, try to assign `t_super` to `r`. If `t_super` is a structured type,
       assign a corresponding structure with fresh meta variables to `r` and then re-invoke sub_type.
       This "grows" the meta variable to match the structure of `t_super`.
  *)
  | TMeta r, _ ->
    (match Types.force t_super with (* Ensure t_super is fully resolved if it's also meta pointing to concrete *)
    | TMeta r' -> (Types.listen r (fun t -> sub_type t t_super); Types.listen r' (fun t' -> sub_type t_sub t'))
    | TBool -> Types.assign r TBool
    | TFin n -> Types.assign r (TFin n)
    | TPair (_, _) -> let a_meta = Types.fresh_meta () in let b_meta = Types.fresh_meta () in
      Types.assign r (TPair (a_meta, b_meta)); sub_type t_sub t_super
    | TFun (_, _) -> let a_meta = Types.fresh_meta () in let b_meta = Types.fresh_meta () in
      Types.assign r (TFun (a_meta, b_meta)); sub_type t_sub t_super
    | TFloat (_, _) -> let b_bag = Bags.fresh_bound_bag () in let c_bag = Bags.fresh_float_bag () in
      Types.assign r (TFloat (b_bag, c_bag)); sub_type t_sub t_super
    | TUnit -> Types.assign r TUnit
    | TList _ -> let elem_meta = Types.fresh_meta () in 
                 Types.assign r (TList elem_meta); sub_type t_sub t_super
    | TRef _ -> let ref_meta = Types.fresh_meta () in
                Types.assign r (TRef ref_meta); sub_type t_sub t_super
    )
  (* Symmetrically, if t_super is a meta variable `r`:
     - If t_sub is also a meta `r'`, listeners are set up (covered by previous case if Types.force makes them same).
     - Otherwise, try to assign `t_sub` to `r`. If `t_sub` is structured,
       assign a corresponding structure with fresh metas to `r` and re-invoke sub_type.
  *)
  | _, TMeta r ->
    (match Types.force t_sub with (* Ensure t_sub is fully resolved *)
    | TMeta r' -> (Types.listen r (fun t -> sub_type t_sub t); Types.listen r' (fun t' -> sub_type t_sub t')) (* Should be covered already *)
    | TBool -> Types.assign r TBool
    | TFin n -> Types.assign r (TFin n)
    | TPair (_, _) -> let a_meta = Types.fresh_meta () in let b_meta = Types.fresh_meta () in
      Types.assign r (TPair (a_meta, b_meta)); sub_type t_sub t_super
    | TFun (_, _) -> let a_meta = Types.fresh_meta () in let b_meta = Types.fresh_meta () in
      Types.assign r (TFun (a_meta, b_meta)); sub_type t_sub t_super
    | TFloat (_, _) -> let b_bag = Bags.fresh_bound_bag () in let c_bag = Bags.fresh_float_bag () in
      Types.assign r (TFloat (b_bag, c_bag)); sub_type t_sub t_super
    | TUnit -> Types.assign r TUnit
    | TList _ -> let elem_meta = Types.fresh_meta () in 
                 Types.assign r (TList elem_meta); sub_type t_sub t_super
    | TRef _ -> let ref_meta = Types.fresh_meta () in
                Types.assign r (TRef ref_meta); sub_type t_sub t_super
    )
  (* Error Case: If none of the above rules match, the types are incompatible. *) 
  | _, _ -> 
      let msg = Printf.sprintf "Type mismatch: cannot subtype %s <: %s"
        (Pretty.string_of_ty t_sub) (Pretty.string_of_ty t_super)
      in
      failwith msg

(** [unify t1 t2] enforces that types [t1] and [t2] are equivalent.
    It achieves this by checking for subtyping in both directions:
    `sub_type t1 t2` and `sub_type t2 t1`.
    If either subtyping check fails, unification fails. The error message
    provides context about the unification attempt and the underlying subtyping error.
*)
and unify (t1 : ty) (t2 : ty) : unit =
  try 
    sub_type t1 t2; 
    sub_type t2 t1
  with Failure msg -> 
    (* Enhance error message for unification failure *)
    let unified_msg = Printf.sprintf "Type mismatch: cannot unify %s and %s\n(Subtyping error: %s)"
      (Pretty.string_of_ty t1) (Pretty.string_of_ty t2) msg
    in
    failwith unified_msg

(** [infer e] is the main entry point for type inference and elaboration.
    It takes an expression [e] of type `expr` and returns a typed expression
    [texpr] (a pair of its inferred type `ty` and an elaborated AST node `taexpr_node`).
    It initializes an empty environment and calls the recursive helper `aux`.
*)
let infer (e : expr) : texpr =
  (* [aux env e_node] recursively infers types and elaborates expressions.
     `env` is a map from variable names (string) to their types (`ty`).
     `e_node` is the current expression node being processed.
  *)
  let rec aux (env : ty StringMap.t) (ExprNode e_node : expr) : texpr =
    match e_node with
    | Const f -> 
      (* For a float constant `f`:
         - `bounds_bag_ref`: A new `BoundBag` is created, initially empty (`Finite BoundSet.empty`).
           This bag will collect comparison points relevant to this constant from its context.
         - `consts_bag_ref`: A new `FloatBag` is created, initialized to `Finite (FloatSet.singleton f)`.
           This signifies that this expression's value is known to be exactly `f`.
         The inferred type is `TFloat (bounds_bag_ref, consts_bag_ref)`.
      *)
      let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in 
      let consts_bag_ref = Bags.FloatBag.create (Finite (FloatSet.singleton f)) in
      (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (Const f))
    | BoolConst b ->
      (TBool, TAExprNode (BoolConst b))
    | Var x ->
      (* For a variable `x`: Look up its type in the current environment `env`.
         If not found, it's an unbound variable error.
      *)
      (try 
        let ty = StringMap.find x env in
        (ty, TAExprNode (Var x))
       with Not_found -> 
        failwith ("Unbound variable: " ^ x))

    | Let (x, e1, e2) ->
      (* For `Let x = e1 In e2`:
         1. Infer type `t1` for `e1` in the current environment `env`.
         2. Create a new environment `env'` by adding the binding `x : t1` to `env`.
         3. Infer type `t2` for `e2` in the extended environment `env'`.
         The result type is `t2`.
      *)
      let t1, a1 = aux env e1 in
      let env' = StringMap.add x t1 env in
      let t2, a2 = aux env' e2 in
      (t2, TAExprNode (Let (x, (t1,a1), (t2,a2))))

    | Sample dist_exp ->
      (* For `Sample dist_exp`:
         - `bounds_bag_ref`: A fresh `BoundBag` for the result of the sample, initially empty.
           This will collect bounds from comparisons involving this sample.
         - `consts_bag_ref`: A fresh `FloatBag` for the result, initialized to `Top`.
           This signifies the sample's output is not a single known constant.
      *)
      let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in 
      let consts_bag_ref = Bags.FloatBag.create Top in 

      (* [add_floats_to_boundbag float_bag bound_bag]:
         Sets up a listener on `float_bag` (an argument's ConstsBag).
         When `float_bag` changes:
         - If it becomes `Finite s` (set of floats), all floats in `s` are added
           as `LessEq` bounds to `bound_bag` (the argument's own BoundBag).
           This means if an argument might be `Const 1.0` or `Const 2.0`, its
           own context should be aware of potential comparisons at `1.0` and `2.0`.
         - If it becomes `Top`, `bound_bag` also becomes `Top`.
      *)
      let add_floats_to_boundbag (float_bag : FloatBag.bag) (bound_bag : BoundBag.bag) =
        let listener () =
          let v = Bags.FloatBag.get float_bag in
          (match v with
          | Finite s -> Bags.BoundBag.add_all s bound_bag (* add_all converts floats to LessEq bounds *)
            | Top -> Bags.BoundBag.leq (Bags.BoundBag.create Top) bound_bag)
        in
        Bags.FloatBag.listen float_bag listener
      in

      (* [make_output_top_if_input_boundbag_is_top input_bound_bag]:
         Sets up a listener on `input_bound_bag` (an argument's BoundBag).
         If `input_bound_bag` becomes `Top` (meaning the argument's discretization context
         is unconstrained), then the main `bounds_bag_ref` (for the Sample's result)
         also becomes `Top`.
      *)
      let make_output_top_if_input_boundbag_is_top input_bound_bag =
        let listener () =
          let v = Bags.BoundBag.get input_bound_bag in
          (match v with
          | Top -> Bags.BoundBag.leq (Bags.BoundBag.create Top) bounds_bag_ref
          | _ -> ())
        in
        Bags.BoundBag.listen input_bound_bag listener
      in

      (* [make_input_top_if_output_boundbag_is_top input_bound_bag output_bound_bag]:
         Sets up a listener on `output_bound_bag` (the Sample's result BoundBag).
         If `output_bound_bag` becomes `Top` (e.g., due to comparison with a `Top` float),
         then `input_bound_bag` (the argument's BoundBag) also becomes `Top`.
         This ensures bidirectionality in propagating the `Top` state for bounds.
      *)
      let make_input_top_if_output_boundbag_is_top input_bound_bag output_bound_bag =
        let listener () =
          let v = Bags.BoundBag.get output_bound_bag in
          (match v with
          | Top -> Bags.BoundBag.leq (Bags.BoundBag.create Top) input_bound_bag
          | _ -> ())
        in
        Bags.BoundBag.listen output_bound_bag listener
      in

      (match dist_exp with
      | Distr1 (dist_kind, arg_e) ->
          let t_arg, a_arg = aux env arg_e in
          (* Arguments to distributions must be floats. Create fresh bags for them. *)
          let t_arg_bound_bag = Bags.fresh_bound_bag () in
          let t_arg_float_bag = Bags.fresh_float_bag () in
          (try unify t_arg (Types.TFloat (t_arg_bound_bag, t_arg_float_bag))
           with Failure msg -> 
            let kind_str = Pretty.string_of_expr_indented (ExprNode (Sample (Distr1 (dist_kind, arg_e)))) in
            failwith (Printf.sprintf "Type error in Sample (%s) argument: %s" kind_str msg));
          
          (* Set up listeners for bag propagation *)
          add_floats_to_boundbag t_arg_float_bag t_arg_bound_bag;
          make_output_top_if_input_boundbag_is_top t_arg_bound_bag;
          make_input_top_if_output_boundbag_is_top t_arg_bound_bag bounds_bag_ref;

          let dist_exp' = Distr1 (dist_kind, (t_arg, a_arg)) in
          (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (Sample dist_exp'))

      | Distr2 (dist_kind, arg1_e, arg2_e) ->
        let t1, a1 = aux env arg1_e in
        let t2, a2 = aux env arg2_e in
        let t1_bound_bag = Bags.fresh_bound_bag () in
        let t1_float_bag = Bags.fresh_float_bag () in
        let t2_bound_bag = Bags.fresh_bound_bag () in
        let t2_float_bag = Bags.fresh_float_bag () in

        (try unify t1 (Types.TFloat (t1_bound_bag, t1_float_bag))
         with Failure msg -> 
          let kind_str = Pretty.string_of_expr_indented (ExprNode (Sample (Distr2 (dist_kind, arg1_e, arg2_e)))) in
          failwith (Printf.sprintf "Type error in Sample (%s) first argument: %s" kind_str msg));
        (try unify t2 (Types.TFloat (t2_bound_bag, t2_float_bag))
         with Failure msg -> 
          let kind_str = Pretty.string_of_expr_indented (ExprNode (Sample (Distr2 (dist_kind, arg1_e, arg2_e)))) in
          failwith (Printf.sprintf "Type error in Sample (%s) second argument: %s" kind_str msg));
        
        add_floats_to_boundbag t1_float_bag t1_bound_bag;
        add_floats_to_boundbag t2_float_bag t2_bound_bag;

        make_output_top_if_input_boundbag_is_top t1_bound_bag;
        make_output_top_if_input_boundbag_is_top t2_bound_bag;

        make_input_top_if_output_boundbag_is_top t1_bound_bag bounds_bag_ref;
        make_input_top_if_output_boundbag_is_top t2_bound_bag bounds_bag_ref;

        let dist_exp' = Distr2 (dist_kind, (t1, a1), (t2, a2)) in
        (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (Sample dist_exp'))
      )
      
    | DistrCase cases ->
      if cases = [] then failwith "DistrCase cannot be empty";
      (* Probabilities must sum to 1.0 (approximately) *)
      let probs = List.map snd cases in
      let sum = List.fold_left (+.) 0.0 probs in
      if abs_float (sum -. 1.0) > 0.0001 then
        failwith (Printf.sprintf "DistrCase probabilities must sum to 1.0, got %f" sum);
      
      (* Infer types for all expressions in the cases.
         Unify all branch types by subtyping them to a fresh result meta variable. *)
      let typed_cases = List.map (fun (e, p) -> (aux env e, p)) cases in
      let result_ty = Types.fresh_meta () in 
      List.iter (fun ((branch_ty, _), _) -> 
        try sub_type branch_ty result_ty
        with Failure msg -> failwith ("Type error in DistrCase branches: " ^ msg)
      ) typed_cases;
      
      let annotated_cases = List.map (fun (texpr, prob) -> (texpr, prob)) typed_cases in
      (result_ty, TAExprNode (DistrCase annotated_cases))

    | Cmp (cmp_op, e1, e2, flipped) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        (* `b_meta` is a fresh BoundBag shared by both operands of the comparison.
           This ensures they are discretized in the same context.
           `c_meta1` and `c_meta2` are fresh FloatBags for the constants of e1 and e2. *)
        let b_meta = Bags.fresh_bound_bag () in 
        let c_meta1 = Bags.fresh_float_bag () in
        let c_meta2 = Bags.fresh_float_bag () in
        (try unify t1 (Types.TFloat (b_meta, c_meta1))
         with Failure msg -> failwith (Printf.sprintf "Type error in comparison left operand: %s" msg));
        (try unify t2 (Types.TFloat (b_meta, c_meta2))
         with Failure msg -> failwith (Printf.sprintf "Type error in comparison right operand: %s" msg));

        (* Listener Logic for Propagating Bounds in Comparisons:
           This listener is triggered if the FloatBag of either operand (`c_meta1` or `c_meta2`) changes.
           Its purpose is to update the shared `b_meta` (BoundBag) with relevant comparison points.
        *)
        let listener () =
          let v1 = Bags.FloatBag.get c_meta1 in (* Value of lhs constants bag *)
          let v2 = Bags.FloatBag.get c_meta2 in (* Value of rhs constants bag *)
          match v1, v2 with
          | Top, Top ->
              (* If both operands are `Top` (unconstrained floats), the comparison context `b_meta` also becomes `Top`. *)
              Bags.BoundBag.leq (Bags.BoundBag.create Top) b_meta
          | Finite _, Finite s2 ->
              (* Discrete-Discrete comparison (both operands have finite sets of known constants).
                 The original comment "Only collect the bounds from the right bag" refers to this case.
                 Bounds are derived from `s2` (constants of the RHS expression `e2`).
                 For `e1 < f` or `e1 <= f` where `f` is in `s2`, `f` becomes a relevant bound.
                 These bounds (e.g., `Less f` or `LessEq f`) are added to the shared `b_meta`.
              *)
              let bounds_to_add = ref Bags.BoundSet.empty in
              FloatSet.iter (fun f -> 
                let bound = match cmp_op with
                  | Types.Lt -> Bags.Less f
                  | Types.Le -> Bags.LessEq f
                in
                bounds_to_add := Bags.BoundSet.add bound !bounds_to_add
              ) s2;
              (match Bags.BoundBag.get b_meta with
              | Top -> () 
              | Finite current_set ->
                  let new_set = Bags.BoundSet.union current_set !bounds_to_add in
                  if not (Bags.BoundSet.equal current_set new_set) then
                    let temp_finite_bag = Bags.BoundBag.create (Finite new_set) in
                    Bags.BoundBag.leq temp_finite_bag b_meta
              )
          | _, _ -> (* Covers (Finite s1, Top) and (Top, Finite s2) - Mixed continuous-discrete comparison *)
              (* Bounds are collected from *both* operands if they are Finite.
                 - For constants `f` from `s2` (RHS, `c_meta2`):
                   If op is `Lt` (e.g. `e1 < f`), bound is `Less f`.
                   If op is `Le` (e.g. `e1 <= f`), bound is `LessEq f`.
                 - For constants `f` from `s1` (LHS, `c_meta1`):
                   The bounds are "mirrored". If op is `Lt` (e.g. `f < e2`), effectively `e2 > f`, so bound is `LessEq f` from `e2`'s perspective if `e2` were a var (or `Greater f` for `e1`).
                   If op is `Lt` (e.g. `e1 < e2`, `s1` has `f`), add `LessEq f`. (e.g. if `f < x`, then `x` could be compared with `f` using `f <= x` effectively).
                   If op is `Le` (e.g. `e1 <= e2`, `s1` has `f`), add `Less f`.
                 All collected bounds are added to the shared `b_meta`.
              *)
              let bounds_to_add = ref Bags.BoundSet.empty in
              (match v2 with (* Collect from RHS constants first *)
               | Finite s2 ->
                   FloatSet.iter (fun f -> 
                     let bound = match cmp_op with
                       | Types.Lt -> Bags.Less f
                       | Types.Le -> Bags.LessEq f
                     in
                     bounds_to_add := Bags.BoundSet.add bound !bounds_to_add
                   ) s2
               | Top -> ()
              );
              (match v1 with (* Collect from LHS constants *)
               | Finite s1 ->
                   FloatSet.iter (fun f -> 
                     let bound = match cmp_op with (* Mirrored bound for LHS constant *)
                       | Types.Lt -> Bags.LessEq f (* If f < e2, then f is a LessEq style bound for e2 *)
                       | Types.Le -> Bags.Less f   (* If f <= e2, then f is a Less style bound for e2 *)
                     in
                     bounds_to_add := Bags.BoundSet.add bound !bounds_to_add
                   ) s1
               | Top -> ()
              );
              if not (Bags.BoundSet.is_empty !bounds_to_add) then
                let current_bound_val = Bags.BoundBag.get b_meta in
                match current_bound_val with
                | Top -> () 
                | Finite current_set ->
                    let new_set = Bags.BoundSet.union current_set !bounds_to_add in
                    if not (Bags.BoundSet.equal current_set new_set) then (
                       let temp_finite_bag = Bags.BoundBag.create (Finite new_set) in
                       Bags.BoundBag.leq temp_finite_bag b_meta
                    )
        in
        Bags.FloatBag.listen c_meta1 listener;
        Bags.FloatBag.listen c_meta2 listener;

        (TBool, TAExprNode (Cmp (cmp_op, (t1,a1), (t2,a2), flipped))) (* Result of comparison is TBool *)

    | FinCmp (cmp_op, e1, e2, n, flipped) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinCmp modulus: ==#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Types.TFin n in
      (* Both operands must be of type TFin n *)
      (try sub_type t1 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinCmp (==#%d) left operand: %s" n msg));
      (try sub_type t2 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinCmp (==#%d) right operand: %s" n msg));
      (Types.TBool, TAExprNode (FinCmp (cmp_op, (t1, a1), (t2, a2), n, flipped)))

    | If (e1, e2, e3) ->
      let t1, a1 = aux env e1 in
      (try sub_type t1 Types.TBool (* Condition `e1` must be TBool *) 
       with Failure msg -> failwith ("Type error in If condition: " ^ msg));
      let t2, a2 = aux env e2 in (* Infer type of true branch `e2` *)
      let t3, a3 = aux env e3 in (* Infer type of false branch `e3` *)
      let result_ty = Types.fresh_meta () in (* Result type is a fresh meta variable *)
      (try 
         sub_type t2 result_ty; (* True branch type must be subtype of result type *)
         sub_type t3 result_ty  (* False branch type must be subtype of result type *)
       with Failure msg -> failwith ("Type error in If branches: " ^ msg)); 
      (result_ty, TAExprNode (If ((t1,a1), (t2,a2), (t3,a3))))
      
    | Pair (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (TPair (t1, t2), TAExprNode (Pair ((t1, a1), (t2, a2))))
      
    | First e1 ->
      let t, a = aux env e1 in
      let t1_meta = Types.fresh_meta () in (* Type of first element *)
      let t2_meta = Types.fresh_meta () in (* Type of second element *)
      (try sub_type t (TPair (t1_meta, t2_meta)) (* `e1` must be a pair *)
       with Failure msg -> failwith ("Type error in First (fst): " ^ msg));
      (Types.force t1_meta, TAExprNode (First (t, a))) (* Result is type of first element *)
      
    | Second e1 ->
      let t, a = aux env e1 in
      let t1_meta = Types.fresh_meta () in
      let t2_meta = Types.fresh_meta () in
      (try sub_type t (TPair (t1_meta, t2_meta)) (* `e1` must be a pair *)
       with Failure msg -> failwith ("Type error in Second (snd): " ^ msg));
      (Types.force t2_meta, TAExprNode (Second (t, a))) (* Result is type of second element *)
      
    | Fun (x, e1) ->
      (* For `Fun x -> e1`:
         - `param_type`: A fresh meta variable for the parameter `x`.
         - Infer type `return_type` for body `e1` in environment extended with `x : param_type`.
         Result type is `TFun (param_type, return_type)`.
      *)
      let param_type = Types.fresh_meta () in
      let env' = StringMap.add x param_type env in
      let return_type, a = aux env' e1 in
      (Types.TFun (param_type, return_type), TAExprNode (Fun (x, (return_type, a))))
      
    | FuncApp (e1, e2) ->
      (* For `e1 e2` (function application):
         - Infer type `t_fun` for `e1` (the function) and `t_arg` for `e2` (the argument).
         - `param_ty_expected`: Fresh meta for the expected parameter type of `t_fun`.
         - `result_ty`: Fresh meta for the result type of `t_fun`.
         - Enforce `t_fun <: TFun (param_ty_expected, result_ty)`.
         - Enforce `t_arg <: param_ty_expected`.
         The overall result type is `result_ty`.
      *)
      let t_fun, a_fun = aux env e1 in
      let t_arg, a_arg = aux env e2 in
      let param_ty_expected = Types.fresh_meta () in 
      let result_ty = Types.fresh_meta () in 
      (try 
         sub_type t_fun (Types.TFun (param_ty_expected, result_ty));
         sub_type t_arg param_ty_expected 
       with Failure msg -> failwith ("Type error in function application: " ^ msg));
      (result_ty, TAExprNode (FuncApp ((t_fun, a_fun), (t_arg, a_arg))))
      
    | LoopApp (e1, e2, e3) -> (* e3 is loop count, not typed expr here *)
      let t_fun, a_fun = aux env e1 in
      let t_arg, a_arg = aux env e2 in
      let param_ty_expected = Types.fresh_meta () in 
      let result_ty = Types.fresh_meta () in 
      (try 
          sub_type t_fun (Types.TFun (param_ty_expected, result_ty));
          sub_type t_arg param_ty_expected 
        with Failure msg -> failwith ("Type error in loop application: " ^ msg));
      (result_ty, TAExprNode (LoopApp ((t_fun, a_fun), (t_arg, a_arg), e3)))
       
    | FinConst (k, n) ->
      if k < 0 || k >= n then failwith (Printf.sprintf "Invalid FinConst value: %d#%d. k must be >= 0 and < n." k n);
      (Types.TFin n, TAExprNode (FinConst (k, n)))

    | FinEq (e1, e2, n) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinEq modulus: ==#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Types.TFin n in
      (* Both operands must be of type TFin n *)
      (try sub_type t1 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinEq (==#%d) left operand: %s" n msg));
      (try sub_type t2 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinEq (==#%d) right operand: %s" n msg));
      (Types.TBool, TAExprNode (FinEq ((t1, a1), (t2, a2), n)))

    | And (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (try sub_type t1 Types.TBool
       with Failure msg -> failwith ("Type error in And (&&) left operand: " ^ msg));
      (try sub_type t2 Types.TBool
       with Failure msg -> failwith ("Type error in And (&&) right operand: " ^ msg));
      (TBool, TAExprNode (And ((t1, a1), (t2, a2))))
      
    | Or (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (try sub_type t1 Types.TBool
       with Failure msg -> failwith ("Type error in Or (||) left operand: " ^ msg));
      (try sub_type t2 Types.TBool
       with Failure msg -> failwith ("Type error in Or (||) right operand: " ^ msg));
      (TBool, TAExprNode (Or ((t1, a1), (t2, a2))))

    | Not e1 ->
      let t1, a1 = aux env e1 in
      (try sub_type t1 Types.TBool
       with Failure msg -> failwith ("Type error in Not operand: " ^ msg));
      (TBool, TAExprNode (Not (t1, a1)))

    | Observe e1 ->
      let t1, a1 = aux env e1 in
      (try sub_type t1 TBool (* Argument to Observe must be TBool *)
       with Failure msg -> failwith ("Type error in Observe argument: " ^ msg));
      (TUnit, TAExprNode (Observe (t1, a1))) (* Observe returns TUnit *)

    | Fix (f, x, e_body) -> 
      (* For `Fix f x -> e_body` (fixed-point combinator for recursion):
         - `fun_type_itself`: Fresh meta for the type of `f` (the function itself).
         - `param_type`: Fresh meta for the type of parameter `x`.
         - Infer `body_ret_type` for `e_body` in env extended with `f:fun_type_itself` and `x:param_type`.
         - `actual_fun_type` is `TFun (param_type, body_ret_type)`.
         - Unify `fun_type_itself` with `actual_fun_type`.
         The result type is `fun_type_itself`.
      *)
      let fun_type_itself = Types.fresh_meta () in 
      let param_type = Types.fresh_meta () in      
      let env_body = StringMap.add x param_type (StringMap.add f fun_type_itself env) in
      let body_texpr = aux env_body e_body in
      let body_ret_type = fst body_texpr in
      let actual_fun_type = Types.TFun (param_type, body_ret_type) in
      unify fun_type_itself actual_fun_type;
      (fun_type_itself, TAExprNode (Fix (f, x, body_texpr)))

    | Nil -> 
      (* For `Nil`: Result is `TList elem_ty` where `elem_ty` is a fresh meta variable.
         The element type will be determined by context (e.g., if Cons'd or unified). *)
      let elem_ty = Types.fresh_meta () in
      (TList elem_ty, TAExprNode Nil) 

    | Cons (e_hd, e_tl) ->
      (* For `e_hd :: e_tl`:
         - Infer type `t_hd` for head `e_hd` and `t_tl` for tail `e_tl`.
         - Unify `t_tl` with `TList t_hd` (tail must be a list of head's type).
         Result type is `t_tl`.
      *)
      let t_hd, a_hd = aux env e_hd in
      let t_tl, a_tl = aux env e_tl in
      (try unify t_tl (TList t_hd) 
       with Failure msg -> failwith ("Type error in list construction (::): " ^ msg));
      (t_tl, TAExprNode (Cons ((t_hd, a_hd), (t_tl, a_tl))))

    | MatchList (e_match, e_nil, y, ys, e_cons) ->
      let t_match, a_match = aux env e_match in (* Expression to match on *)
      let elem_ty = Types.fresh_meta () in (* Element type of the list *)
      (try unify t_match (TList elem_ty) (* Must be a list type *)
       with Failure msg -> failwith ("Type error in match expression (expected list type): " ^ msg));
      (* Type check nil branch *) 
      let t_nil, a_nil = aux env e_nil in
      (* Type check cons branch: `y` has `elem_ty`, `ys` has `t_match` (the list type) *)
      let env_cons = StringMap.add y elem_ty (StringMap.add ys t_match env) in
      let t_cons, a_cons = aux env_cons e_cons in
      (* Unify branch types: nil branch and cons branch must have compatible types *)
      let result_ty = Types.fresh_meta () in
      (try 
         sub_type t_nil result_ty;
         sub_type t_cons result_ty
       with Failure msg -> failwith ("Type error in match branches: " ^ msg));
      (result_ty, TAExprNode (MatchList ((t_match, a_match), (t_nil, a_nil), y, ys, (t_cons, a_cons))))

    | Ref e1 ->
      let t1, a1 = aux env e1 in
      (TRef t1, TAExprNode (Ref (t1, a1))) (* Type is TRef of inner type *)

    | Deref e1 ->
      let t1, a1 = aux env e1 in
      let val_ty = Types.fresh_meta () in (* Type of the value stored in the ref *)
      (try unify t1 (TRef val_ty) (* `e1` must be a TRef *)
       with Failure msg -> failwith ("Type error in dereference (!): " ^ msg));
      (Types.force val_ty, TAExprNode (Deref (t1, a1))) (* Result is the type of the value *)

    | Assign (e1, e2) ->
      let t1, a1 = aux env e1 in (* `e1` is the ref *)
      let t2, a2 = aux env e2 in (* `e2` is the value to assign *)
      let val_ty = Types.fresh_meta () in (* Type of value stored in ref *)
      (try 
         unify t1 (TRef val_ty); (* `e1` must be TRef val_ty *)
         sub_type t2 (Types.force val_ty) (* `t2` must be subtype of value type *)
       with Failure msg -> failwith ("Type error in assignment (:=): " ^ msg));
      (TUnit, TAExprNode (Assign ((t1, a1), (t2, a2)))) (* Assignment returns TUnit *)

    | Seq (e1, e2) ->
      let t1, a1 = aux env e1 in (* Typecheck e1 (for effects) *)
      let t2, a2 = aux env e2 in (* Typecheck e2 *)
      (t2, TAExprNode (Seq ((t1, a1), (t2, a2)))) (* Type of sequence is type of e2 *)

    | Unit -> (Types.TUnit, TAExprNode Unit)

    | RuntimeError s -> (Types.fresh_meta (), TAExprNode (RuntimeError s)) (* Can be any type, context will constrain *)

  in
  aux StringMap.empty e

(** [infer_bool e] infers the type of expression [e] using [infer]
    and then enforces that this inferred type is a subtype of `TBool`.
    It is used for expressions in contexts where a boolean is strictly expected,
    like conditions of If statements or Observe statements.
*)
let infer_bool (e : expr) : texpr =
  let t, a = infer e in
  sub_type t Types.TBool;
  (t, a) 