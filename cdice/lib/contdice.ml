(**
 * @file contdice.ml
 * @brief Core implementation of the ContDice language, including type elaboration and discretization.
 *
 * This module serves as the central hub for the ContDice language processing.
 * It integrates various components like parsing, type systems, and AST manipulation.
 * Key functionalities include:
 * - Type Elaboration (`elab`): Infers types for expressions and annotates the AST,
 *   generating a Typed Abstract Syntax Tree (TAST). This process involves
 *   unification and subtyping with constraint propagation using `Bag` data structures
 *   for float bounds and constants.
 * - Discretization (`discretize`): Transforms a typed ContDice expression (TAST)
 *   into an equivalent expression where continuous distributions and comparisons
 *   are potentially converted into discrete counterparts based on inferred type information
 *   (specifically, the float values encountered in comparisons).
 * - Re-exporting of main library modules for external use.
 *)

open Types (* Access to sub-modules: Ast, TypeSystem, TypedAst, RuntimeValues *)
open Ast (* For expr, ExprNode, and AST constructors *)
open TypeSystem (* For ty, TFloat, force, fresh_meta, assign etc. *)
open TypedAst (* For texpr, TAExprNode *)
open Bags (* For FloatSet, BoundSet, FloatBag, BoundBag, etc. *)

(* Re-export internal modules for easier access by library users or tests. *)
module Parse = Parse
module Pretty = Pretty
module Util = Util
module To_dice = To_dice
module Interp = Interp
module Types = Types (* Re-export the main Types module itself (which in turn re-exports Ast, TypeSystem, etc.) *)
module Distributions = Distributions (* Re-export Distributions module *)
module Bags = Bags (* Re-export Bags module *)

(** A string map, commonly used for environments mapping variable names to types or values. *)
module StringMap = Map.Make (String)

(**
 * @function find_index
 * @brief Finds the index of the first element in a list that satisfies a predicate.
 * @param pred The predicate function `('a -> bool)`.
 * @param lst The list to search.
 * @return `Some index` if an element is found, `None` otherwise.
 *)
let find_index pred lst =
  let rec loop i = function
    | [] -> None (* Not found *)
    | x :: xs -> if pred x then Some i else loop (i + 1) xs
  in
  loop 0 lst

(* ======== Helper for Constant Discretization during the `discretize` phase ======== *)

(**
 * @function get_const_idx_and_modulus
 * @brief Calculates the discrete index and modulus for a float constant `f`
 * based on a set of comparison "cuts" (bounds).
 *
 * This function is used in the discretization process. Given a float `f` and a
 * `BoundSet.t` (which represents all float values `c` that `f` might be compared against,
 * e.g., from `x < c`), this function determines which interval `f` falls into.
 * The intervals are defined by the sorted unique values from the `bound_set_from_context`.
 *
 * Example: If bounds are `{<1.0, <5.0}`, cuts are `1.0, 5.0`. Intervals are:
 *   `(-inf, 1.0)` (index 0)
 *   `[1.0, 5.0)` (index 1)
 *   `[5.0, +inf)` (index 2)
 * The modulus is `1 + number_of_cuts = 3`.
 * If `f = 0.5`, it falls in index 0. If `f = 3.0`, index 1. If `f = 6.0`, index 2.
 *
 * @param f The float constant.
 * @param bound_set_from_context A `BoundSet.t` containing `bound` values (e.g., `Less c`, `LessEq c`).
 * @return A pair `(index, modulus)` for the discretized representation of `f`.
 *)
let get_const_idx_and_modulus (f : float) (bound_set_from_context : BoundSet.t)
    : int * int =
  let cuts_as_bounds = BoundSet.elements bound_set_from_context in (* Sorted list of unique bounds *)
  let modulus = 1 + List.length cuts_as_bounds in (* Number of intervals = number of cuts + 1 *)
  (* The discrete index of the float `f` is the count of bounds `b` in `cuts_as_bounds` such that `f` does *not* satisfy `b`.
     Equivalently, it's the index of the *first* bound `b` that `f` *does* satisfy (if any).
     If `f` satisfies no bounds (e.g. `f` is larger than all cut points), it's in the last interval. *)
  let idx =
    find_index (fun bound -> Bags.satisfies_bound f bound) cuts_as_bounds
  in
  match idx with
  | Some i -> i (* `f` satisfies the bound at index `i`, so it belongs to interval `i` *)
  | None -> modulus - 1 (* `f` does not satisfy any bound, so it's in the last interval (index modulus - 1) *)

(* ======== Type System: Subtyping and Unification ======== *)

(**
 * @function sub_type
 * @brief Enforces that `t_sub` is a subtype of `t_super` (t_sub <: t_super).
 * This is a core part of the type elaboration process (`elab`).
 * It handles various type constructors and uses unification for `TMeta` variables.
 *
 * @param t_sub The potential subtype.
 * @param t_super The potential supertype.
 * @unit This function has side effects (assigning meta variables via `unify` or `assign`)
 *       and raises `Failure` on a type mismatch.
 *)
let rec sub_type (t_sub : ty) (t_super : ty) : unit =
  match (force t_sub, force t_super) with (* Types.force -> force *)
  (* Base Cases *)
  | TBool, TBool -> ()
  | TFin n1, TFin n2 when n1 = n2 -> ()
  | TUnit, TUnit -> ()
  (* Structural Cases *)
  | TFloat (b1, c1), TFloat (b2, c2) ->
      Bags.BoundBag.eq b1 b2;
      (* Bounds must be consistent *)
      Bags.FloatBag.leq c1 c2 (* Constants flow sub -> super *)
  | TPair (a1, b1), TPair (a2, b2) ->
      sub_type a1 a2;
      (* Covariant *)
      sub_type b1 b2 (* Covariant *)
  | TFun (a1, b1), TFun (a2, b2) ->
      sub_type a2 a1;
      (* Contravariant argument *)
      sub_type b1 b2 (* Covariant result *)
  | TList t1, TList t2 -> sub_type t1 t2 (* Covariant *)
  | TRef t1, TRef t2 -> unify t1 t2 (* Invariant *)
  | TMeta r, _ -> (
      match force t_super with (* Types.force -> force *)
      (* Ensure t_super is forced *)
      | TMeta r' ->
          listen r (fun t -> sub_type t t_super); (* Types.listen -> listen *)
          listen r' (fun t' -> sub_type t_sub t') (* Types.listen -> listen *)
      | TBool -> assign r TBool (* Types.assign -> assign *)
      | TFin n -> assign r (TFin n) (* Types.assign -> assign *)
      | TPair (_, _) ->
          let a_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          let b_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          assign r (TPair (a_meta, b_meta)); (* Types.assign -> assign *)
          sub_type t_sub t_super
      | TFun (_, _) ->
          let a_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          let b_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          assign r (TFun (a_meta, b_meta)); (* Types.assign -> assign *)
          sub_type t_sub t_super
      | TFloat (_, _) ->
          let b_bag = Bags.fresh_bound_bag () in
          let c_bag = Bags.fresh_float_bag () in
          assign r (TFloat (b_bag, c_bag)); (* Types.assign -> assign *)
          sub_type t_sub t_super
      | TUnit -> assign r TUnit (* Handle TUnit for t_super *) (* Types.assign -> assign *)
      | TList _ ->
          let elem_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          assign r (TList elem_meta); (* Types.assign -> assign *)
          sub_type t_sub t_super
      | TRef _ ->
          let ref_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          assign r (TRef ref_meta); (* Types.assign -> assign *)
          sub_type t_sub t_super)
  | _, TMeta r -> (
      match force t_sub with (* Types.force -> force *)
      (* Ensure t_sub is forced *)
      | TMeta r' ->
          listen r (fun t -> sub_type t_sub t); (* Types.listen -> listen *)
          listen r' (fun t' -> sub_type t_sub t') (* Types.listen -> listen *)
      | TBool -> assign r TBool (* Types.assign -> assign *)
      | TFin n -> assign r (TFin n) (* Types.assign -> assign *)
      | TPair (_, _) ->
          let a_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          let b_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          assign r (TPair (a_meta, b_meta)); (* Types.assign -> assign *)
          sub_type t_sub t_super
      | TFun (_, _) ->
          let a_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          let b_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          assign r (TFun (a_meta, b_meta)); (* Types.assign -> assign *)
          sub_type t_sub t_super
      | TFloat (_, _) ->
          let b_bag = Bags.fresh_bound_bag () in
          let c_bag = Bags.fresh_float_bag () in
          assign r (TFloat (b_bag, c_bag)); (* Types.assign -> assign *)
          sub_type t_sub t_super
      | TUnit -> assign r TUnit (* Handle TUnit for t_sub *) (* Types.assign -> assign *)
      | TList _ ->
          let elem_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          assign r (TList elem_meta); (* Types.assign -> assign *)
          sub_type t_sub t_super
      | TRef _ ->
          let ref_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
          assign r (TRef ref_meta); (* Types.assign -> assign *)
          sub_type t_sub t_super)
  (* Error Case *)
  | _, _ ->
      let msg =
        Printf.sprintf "Type mismatch: cannot subtype %s <: %s"
          (Pretty.string_of_ty t_sub)
          (Pretty.string_of_ty t_super)
      in
      failwith msg

(**
 * @function unify
 * @brief Enforces type equality between `t1` and `t2` (t1 = t2).
 * This is achieved by requiring `t1 <: t2` and `t2 <: t1`.
 *
 * @param t1 The first type.
 * @param t2 The second type.
 * @unit Raises `Failure` if the types cannot be unified.
 *)
and unify (t1 : ty) (t2 : ty) : unit =
  try
    sub_type t1 t2;
    sub_type t2 t1
  with Failure msg ->
    (* Provide a more specific error message for unification failures. *)
    let unified_msg =
      Printf.sprintf
        "Type mismatch: cannot unify %s and %s\n(Underlying subtyping error: %s)"
        (Pretty.string_of_ty t1) (Pretty.string_of_ty t2) msg
    in
    failwith unified_msg

(* ======== Type Elaboration: AST to TAST ======== *)

(**
 * @function elab
 * @brief Main type elaboration function. Converts an untyped `Ast.expr` into a
 * typed `TypedAst.texpr` (Typed Abstract Syntax Tree).
 *
 * This function traverses the AST, infers types for each node, checks for type
 * consistency, and propagates constraints using `Bag`s for float types.
 * It builds an environment (`ty StringMap.t`) mapping variable names to their inferred types.
 *
 * @param e The input expression (`Ast.expr`) to elaborate.
 * @return The elaborated typed expression (`TypedAst.texpr`).
 * @raise Failure on type errors or if variables are unbound.
 *)
let elab (e : expr) : texpr =
  (**
   * @function aux
   * @brief Recursive helper for `elab`.
   * @param env Current typing environment (maps variable names to `TypeSystem.ty`).
   * @param expr_node_wrapper The `Ast.ExprNode` containing the actual `expr_generic` node.
   * @return The `TypedAst.texpr` for the elaborated sub-expression.
   *)
  let rec aux (env : ty StringMap.t) (ExprNode e_node : expr) : texpr =
    match e_node with
    | Const f ->
        (* For a float constant, create fresh bags for its bounds and constants.
           The constant itself is added to its FloatBag. *)
        let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in (* Initially no bounds *)
        let consts_bag_ref = Bags.FloatBag.create (Finite (FloatSet.singleton f)) in (* Contains only f *)
        (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (Const f))

    | BoolConst b -> (TBool, TAExprNode (BoolConst b))

    | Var x -> (
        try
          let ty = StringMap.find x env in (* Look up variable type in environment *)
          (ty, TAExprNode (Var x))
        with Not_found -> failwith ("Unbound variable during elaboration: " ^ x))

    | Let (x, e1, e2) ->
        let t1, a1 = aux env e1 in (* Elaborate e1 *)
        let env' = StringMap.add x t1 env in (* Extend environment with x:t1 *)
        let t2, a2 = aux env' e2 in (* Elaborate e2 in extended environment *)
        (t2, TAExprNode (Let (x, (t1, a1), (t2, a2)))) (* Result type is type of e2 *)

    | Sample dist_exp -> (
        (* For sampling, the result is a float. Create fresh bags for its type.
           The constant bag is Top initially, as the sampled value isn't a fixed constant.
           The bounds bag is also initially empty. Constraints will be added by comparisons. *)
        let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in
        let consts_bag_ref = Bags.FloatBag.create Top in (* Sampled value is not a compile-time constant *)

        (* Helper: Propagates float constants from an argument's FloatBag to its own BoundBag.
           If `arg_float_bag` becomes `Finite {c1, c2}`, then `arg_bound_bag` gets constraints
           like `<=c1`, `<=c2`. If `arg_float_bag` becomes `Top`, `arg_bound_bag` becomes `Top`. *)
        let add_floats_to_boundbag (arg_float_bag : FloatBag.bag) (arg_bound_bag : BoundBag.bag) =
          let listener () =
            match Bags.FloatBag.get arg_float_bag with
            | Finite s -> Bags.BoundBag.add_all s arg_bound_bag (* Add all constants as LessEq bounds *)
            | Top -> Bags.BoundBag.leq (Bags.BoundBag.create Top) arg_bound_bag (* Propagate Top *)
          in
          Bags.FloatBag.listen arg_float_bag listener
        in

        (* Helper: If an input argument's BoundBag becomes Top (unconstrained by comparisons),
           it implies the output distribution's bounds might also be unconstrained relative to it.
           This ensures that if an input used in a comparison that defines the output's bounds
           becomes Top, the output's bounds reflect this lack of constraint. *)
        let make_output_top_if_input_boundbag_is_top input_arg_bound_bag =
          let listener () =
            if Bags.BoundBag.get input_arg_bound_bag = Top then
              Bags.BoundBag.leq (Bags.BoundBag.create Top) bounds_bag_ref (* bounds_bag_ref is for the Sample node itself *)
          in
          Bags.BoundBag.listen input_arg_bound_bag listener
        in
        
        (* Helper: Bidirectional constraint. If the output distribution's bounds become Top,
           it might imply that the input parameter's effective bounds (from its usage elsewhere)
           should also be considered Top in the context of this specific Sample operation.
           This is a more aggressive propagation. *)
        let make_input_top_if_output_boundbag_is_top input_arg_bound_bag output_dist_bound_bag =
          let listener () =
            if Bags.BoundBag.get output_dist_bound_bag = Top then
                 Bags.BoundBag.leq (Bags.BoundBag.create Top) input_arg_bound_bag
          in
          Bags.BoundBag.listen output_dist_bound_bag listener
        in

        match dist_exp with
        | Distr1 (dist_kind, arg_e) -> (* Single-argument distribution, e.g., exponential(rate) *)
            let t_arg, a_arg = aux env arg_e in (* Elaborate the argument expression *)
            let t_arg_bound_bag = Bags.fresh_bound_bag () in (* Fresh bags for the argument's expected float type *)
            let t_arg_float_bag = Bags.fresh_float_bag () in
            (try unify t_arg (TFloat (t_arg_bound_bag, t_arg_float_bag)) (* Types.TFloat -> TFloat *)
             with Failure msg ->
               let kind_str =
                 Pretty.string_of_expr_indented
                   (ExprNode (Sample (Distr1 (dist_kind, arg_e))))
               in
               (* Get a string for the kind *)
               failwith
                 (Printf.sprintf "Type error in Sample (%s) argument: %s"
                    kind_str msg));

            add_floats_to_boundbag t_arg_float_bag t_arg_bound_bag;
            make_output_top_if_input_boundbag_is_top t_arg_bound_bag;
            make_input_top_if_output_boundbag_is_top t_arg_bound_bag
              bounds_bag_ref;

            let dist_exp' = Distr1 (dist_kind, (t_arg, a_arg)) in
            ( TFloat (bounds_bag_ref, consts_bag_ref),
              TAExprNode (Sample dist_exp') )
        | Distr2 (dist_kind, arg1_e, arg2_e) ->
            let t1, a1 = aux env arg1_e in
            let t2, a2 = aux env arg2_e in
            let t1_bound_bag = Bags.fresh_bound_bag () in
            let t1_float_bag = Bags.fresh_float_bag () in
            let t2_bound_bag = Bags.fresh_bound_bag () in
            let t2_float_bag = Bags.fresh_float_bag () in

            (try unify t1 (TFloat (t1_bound_bag, t1_float_bag)) (* Types.TFloat -> TFloat *)
             with Failure msg ->
               let kind_str =
                 Pretty.string_of_expr_indented
                   (ExprNode (Sample (Distr2 (dist_kind, arg1_e, arg2_e))))
               in
               failwith
                 (Printf.sprintf "Type error in Sample (%s) first argument: %s"
                    kind_str msg));
            (try unify t2 (TFloat (t2_bound_bag, t2_float_bag)) (* Types.TFloat -> TFloat *)
             with Failure msg ->
               let kind_str =
                 Pretty.string_of_expr_indented
                   (ExprNode (Sample (Distr2 (dist_kind, arg1_e, arg2_e))))
               in
               failwith
                 (Printf.sprintf "Type error in Sample (%s) second argument: %s"
                    kind_str msg));

            add_floats_to_boundbag t1_float_bag t1_bound_bag;
            add_floats_to_boundbag t2_float_bag t2_bound_bag;

            make_output_top_if_input_boundbag_is_top t1_bound_bag;
            make_output_top_if_input_boundbag_is_top t2_bound_bag;

            make_input_top_if_output_boundbag_is_top t1_bound_bag bounds_bag_ref;
            make_input_top_if_output_boundbag_is_top t2_bound_bag bounds_bag_ref;

            let dist_exp' = Distr2 (dist_kind, (t1, a1), (t2, a2)) in
            ( TFloat (bounds_bag_ref, consts_bag_ref),
              TAExprNode (Sample dist_exp') ))
    | DistrCase cases ->
        if cases = [] then failwith "DistrCase cannot be empty";
        (* Check probabilities sum to 1 *)
        let probs = List.map snd cases in
        let sum = List.fold_left ( +. ) 0.0 probs in
        if abs_float (sum -. 1.0) > 0.0001 then
          failwith
            (Printf.sprintf "DistrCase probabilities must sum to 1.0, got %f"
               sum);

        (* Type-check all expressions and subtype them into a fresh result
           type *)
        let typed_cases = List.map (fun (e, p) -> (aux env e, p)) cases in
        let result_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (* Fresh meta for the result *)
        List.iter
          (fun ((branch_ty, _), _) ->
            try sub_type branch_ty result_ty (* Enforce branch <: result *)
            with Failure msg ->
              failwith ("Type error in DistrCase branches: " ^ msg))
          typed_cases;

        let annotated_cases =
          List.map (fun (texpr, prob) -> (texpr, prob)) typed_cases
        in
        (result_ty, TAExprNode (DistrCase annotated_cases))
    | Less (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let b_meta = Bags.fresh_bound_bag () in
        (* Shared bound bag for unification *)
        let c_meta1 = Bags.fresh_float_bag () in
        let c_meta2 = Bags.fresh_float_bag () in
        (try
           unify t1 (TFloat (b_meta, c_meta1)) (* Types.TFloat -> TFloat *)
           (* Unify t1 with TFloat(b_meta, c1) *)
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in Less (<) left operand: %s" msg));
        (try
           unify t2 (TFloat (b_meta, c_meta2)) (* Types.TFloat -> TFloat *)
           (* Unify t2 with TFloat(b_meta, c2) *)
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in Less (<) right operand: %s" msg));

        (* Nested listener logic for Less *)
        let listener () =
          (* Listener takes unit *)
          let v1 = Bags.FloatBag.get c_meta1 in
          (* Get value inside listener *)
          let v2 = Bags.FloatBag.get c_meta2 in
          (* Get value inside listener *)
          match (v1, v2) with
          | Top, Top ->
              (* Both Top -> BoundBag should be Top *)
              Bags.BoundBag.leq (Bags.BoundBag.create Top) b_meta
          | Finite _, Finite s2 -> (
              (* Both are not Top. This means that e2 is being compared to a
                 discrete distribution. *)
              (* Only collect the bounds from the right bag, the constant itself *)
              (* Temporarily store bounds to add *)
              let bounds_to_add = ref Bags.BoundSet.empty in

              FloatSet.iter
                (fun f ->
                  bounds_to_add :=
                    Bags.BoundSet.add (Bags.Less f) !bounds_to_add)
                s2;

              (* Apply collected bounds to b_meta *)
              match Bags.BoundBag.get b_meta with
              | Top -> () (* Cannot add to Top *)
              | Finite current_set ->
                  let new_set =
                    Bags.BoundSet.union current_set !bounds_to_add
                  in
                  if not (Bags.BoundSet.equal current_set new_set) then
                    let temp_finite_bag =
                      Bags.BoundBag.create (Finite new_set)
                    in
                    Bags.BoundBag.leq temp_finite_bag b_meta)
          | _, _ -> (
              (* Only one is Top. This means that e2 is being compared to a
                 continuous distribution. *)
              (* Add bounds from Finite bags. *)
              (* Temporarily store bounds to add *)
              let bounds_to_add = ref Bags.BoundSet.empty in

              (* Collect bounds from right bag (c_meta2) - LessEq *)
              (match v2 with
              | Finite s2 ->
                  FloatSet.iter
                    (fun f ->
                      bounds_to_add :=
                        Bags.BoundSet.add (Bags.Less f) !bounds_to_add)
                    s2
              | Top -> ());

              (* Collect bounds from left bag (c_meta1) - Less *)
              (match v1 with
              | Finite s1 ->
                  FloatSet.iter
                    (fun f ->
                      bounds_to_add :=
                        Bags.BoundSet.add (Bags.LessEq f) !bounds_to_add)
                    s1
              | Top -> ());

              (* Apply collected bounds to b_meta *)
              if not (Bags.BoundSet.is_empty !bounds_to_add) then
                let current_bound_val = Bags.BoundBag.get b_meta in
                match current_bound_val with
                | Top -> () (* Cannot add to Top *)
                | Finite current_set ->
                    let new_set =
                      Bags.BoundSet.union current_set !bounds_to_add
                    in
                    if not (Bags.BoundSet.equal current_set new_set) then
                      (* Update using temporary bag and leq *)
                      let temp_finite_bag =
                        Bags.BoundBag.create (Finite new_set)
                      in
                      Bags.BoundBag.leq temp_finite_bag b_meta)
        in
        (* Register the combined listener on both float bags *)
        Bags.FloatBag.listen c_meta1 listener;
        Bags.FloatBag.listen c_meta2 listener;

        (TBool, TAExprNode (Less ((t1, a1), (t2, a2))))
        (* Result is TBool *)
    | LessEq (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let b_meta = Bags.fresh_bound_bag () in
        (* Shared bound bag for unification *)
        let c_meta1 = Bags.fresh_float_bag () in
        let c_meta2 = Bags.fresh_float_bag () in
        (try
           unify t1 (TFloat (b_meta, c_meta1)) (* Types.TFloat -> TFloat *)
           (* Unify t1 with TFloat(b_meta, c1) *)
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in Less (<) left operand: %s" msg));
        (try
           unify t2 (TFloat (b_meta, c_meta2)) (* Types.TFloat -> TFloat *)
           (* Unify t2 with TFloat(b_meta, c2) *)
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in Less (<) right operand: %s" msg));

        (* Nested listener logic for LessEq *)
        let listener () =
          (* Listener takes unit *)
          let v1 = Bags.FloatBag.get c_meta1 in
          (* Get value inside listener *)
          let v2 = Bags.FloatBag.get c_meta2 in
          (* Get value inside listener *)
          match (v1, v2) with
          | Top, Top ->
              (* Both Top -> BoundBag should be Top *)
              Bags.BoundBag.leq (Bags.BoundBag.create Top) b_meta
          | Finite _, Finite s2 -> (
              (* Both are not Top. This means that e2 is being compared to a
                 discrete distribution. *)
              (* Only collect the bounds from the right bag, the constant itself *)
              (* Temporarily store bounds to add *)
              let bounds_to_add = ref Bags.BoundSet.empty in

              FloatSet.iter
                (fun f ->
                  bounds_to_add :=
                    Bags.BoundSet.add (Bags.LessEq f) !bounds_to_add)
                s2;

              (* Apply collected bounds to b_meta *)
              match Bags.BoundBag.get b_meta with
              | Top -> () (* Cannot add to Top *)
              | Finite current_set ->
                  let new_set =
                    Bags.BoundSet.union current_set !bounds_to_add
                  in
                  if not (Bags.BoundSet.equal current_set new_set) then
                    let temp_finite_bag =
                      Bags.BoundBag.create (Finite new_set)
                    in
                    Bags.BoundBag.leq temp_finite_bag b_meta)
          | _, _ -> (
              (* Only one is Top. This means that e2 is being compared to a
                 continuous distribution. *)
              (* Add bounds from Finite bags. *)
              (* Temporarily store bounds to add *)
              let bounds_to_add = ref Bags.BoundSet.empty in

              (* Collect bounds from right bag (c_meta2) - LessEq *)
              (match v2 with
              | Finite s2 ->
                  FloatSet.iter
                    (fun f ->
                      bounds_to_add :=
                        Bags.BoundSet.add (Bags.LessEq f) !bounds_to_add)
                    s2
              | Top -> ());

              (* Collect bounds from left bag (c_meta1) - Less *)
              (match v1 with
              | Finite s1 ->
                  FloatSet.iter
                    (fun f ->
                      bounds_to_add :=
                        Bags.BoundSet.add (Bags.Less f) !bounds_to_add)
                    s1
              | Top -> ());

              (* Apply collected bounds to b_meta *)
              if not (Bags.BoundSet.is_empty !bounds_to_add) then
                let current_bound_val = Bags.BoundBag.get b_meta in
                match current_bound_val with
                | Top -> () (* Cannot add to Top *)
                | Finite current_set ->
                    let new_set =
                      Bags.BoundSet.union current_set !bounds_to_add
                    in
                    if not (Bags.BoundSet.equal current_set new_set) then
                      (* Update using temporary bag and leq *)
                      let temp_finite_bag =
                        Bags.BoundBag.create (Finite new_set)
                      in
                      Bags.BoundBag.leq temp_finite_bag b_meta)
        in
        (* Register the combined listener on both float bags *)
        Bags.FloatBag.listen c_meta1 listener;
        Bags.FloatBag.listen c_meta2 listener;

        (TBool, TAExprNode (LessEq ((t1, a1), (t2, a2))))
        (* Result is TBool *)
    | If (e1, e2, e3) ->
        let t1, a1 = aux env e1 in
        (try sub_type t1 TBool (* Condition must be bool *) (* Types.TBool -> TBool *)
         with Failure msg -> failwith ("Type error in If condition: " ^ msg));
        let t2, a2 = aux env e2 in
        let t3, a3 = aux env e3 in
        let result_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (* Fresh meta for the result *)
        (try
           sub_type t2 result_ty;
           (* Enforce true_branch <: result *)
           sub_type t3 result_ty (* Enforce false_branch <: result *)
         with Failure msg -> failwith ("Type error in If branches: " ^ msg));
        (result_ty, TAExprNode (If ((t1, a1), (t2, a2), (t3, a3))))
    | Pair (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        (TPair (t1, t2), TAExprNode (Pair ((t1, a1), (t2, a2))))
    | First e1 ->
        let t, a = aux env e1 in
        let t1_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        let t2_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (try sub_type t (TPair (t1_meta, t2_meta))
         with Failure msg -> failwith ("Type error in First (fst): " ^ msg));
        (force t1_meta, TAExprNode (First (t, a))) (* Types.force -> force *)
        (* Use Types.force *)
    | Second e1 ->
        let t, a = aux env e1 in
        let t1_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        let t2_meta = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (try sub_type t (TPair (t1_meta, t2_meta))
         with Failure msg -> failwith ("Type error in Second (snd): " ^ msg));
        (force t2_meta, TAExprNode (Second (t, a))) (* Types.force -> force *)
        (* Use Types.force *)
    | Fun (x, e1) ->
        let param_type = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        let env' = StringMap.add x param_type env in
        let return_type, a = aux env' e1 in
        ( TFun (param_type, return_type), (* Types.TFun -> TFun *)
          TAExprNode (Fun (x, (return_type, a))) )
    | FuncApp (e1, e2) ->
        let t_fun, a_fun = aux env e1 in
        let t_arg, a_arg = aux env e2 in
        let param_ty_expected = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (* Fresh meta for expected param type *)
        let result_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (* Fresh meta for result type *)
        (try
           (* Check t_fun is a function expecting param_ty_expected and
              returning result_ty *)
           sub_type t_fun (TFun (param_ty_expected, result_ty)); (* Types.TFun -> TFun *)
           (* Check t_arg is a subtype of what the function expects *)
           sub_type t_arg param_ty_expected
         with Failure msg ->
           failwith ("Type error in function application: " ^ msg));
        (result_ty, TAExprNode (FuncApp ((t_fun, a_fun), (t_arg, a_arg))))
    | LoopApp (e1, e2, e3) ->
        let t_fun, a_fun = aux env e1 in
        let t_arg, a_arg = aux env e2 in
        (* Third argument is just a number *)
        let param_ty_expected = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (* Fresh meta for expected param type *)
        let result_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (* Fresh meta for result type *)
        (try
           (* Check t_fun is a function expecting param_ty_expected and
              returning result_ty *)
           sub_type t_fun (TFun (param_ty_expected, result_ty)); (* Types.TFun -> TFun *)
           (* Check t_arg is a subtype of what the function expects *)
           sub_type t_arg param_ty_expected
         with Failure msg ->
           failwith ("Type error in loop application: " ^ msg));
        (result_ty, TAExprNode (LoopApp ((t_fun, a_fun), (t_arg, a_arg), e3)))
    | FinConst (k, n) ->
        if k < 0 || k >= n then
          failwith
            (Printf.sprintf
               "Invalid FinConst value: %d#%d. k must be >= 0 and < n." k n);
        (TFin n, TAExprNode (FinConst (k, n))) (* Types.TFin -> TFin *)
    | FinLt (e1, e2, n) ->
        if n <= 0 then
          failwith
            (Printf.sprintf "Invalid FinLt modulus: <#%d. n must be > 0." n);
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let expected_type = TFin n in (* Types.TFin -> TFin *)
        (try sub_type t1 expected_type
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in FinLt (<#%d) left operand: %s" n msg));
        (try sub_type t2 expected_type
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in FinLt (<#%d) right operand: %s" n
                msg));
        (TBool, TAExprNode (FinLt ((t1, a1), (t2, a2), n))) (* Types.TBool -> TBool *)
    | FinLeq (e1, e2, n) ->
        if n <= 0 then
          failwith
            (Printf.sprintf "Invalid FinLeq modulus: <=#%d. n must be > 0." n);
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let expected_type = TFin n in (* Types.TFin -> TFin *)
        (try sub_type t1 expected_type
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in FinLeq (<=#%d) left operand: %s" n
                msg));
        (try sub_type t2 expected_type
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in FinLeq (<=#%d) right operand: %s" n
                msg));
        (TBool, TAExprNode (FinLeq ((t1, a1), (t2, a2), n))) (* Types.TBool -> TBool *)
    | FinEq (e1, e2, n) ->
        (* New case for FinEq in elab *)
        if n <= 0 then
          failwith
            (Printf.sprintf "Invalid FinEq modulus: ==#%d. n must be > 0." n);
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let expected_type = TFin n in (* Types.TFin -> TFin *)
        (try sub_type t1 expected_type
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in FinEq (==#%d) left operand: %s" n
                msg));
        (try sub_type t2 expected_type
         with Failure msg ->
           failwith
             (Printf.sprintf "Type error in FinEq (==#%d) right operand: %s" n
                msg));
        (TBool, TAExprNode (FinEq ((t1, a1), (t2, a2), n))) (* Types.TBool -> TBool *)
    | And (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        (try sub_type t1 TBool (* Types.TBool -> TBool *)
         with Failure msg ->
           failwith ("Type error in And (&&) left operand: " ^ msg));
        (try sub_type t2 TBool (* Types.TBool -> TBool *)
         with Failure msg ->
           failwith ("Type error in And (&&) right operand: " ^ msg));
        (TBool, TAExprNode (And ((t1, a1), (t2, a2))))
    | Or (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        (try sub_type t1 TBool (* Types.TBool -> TBool *)
         with Failure msg ->
           failwith ("Type error in Or (||) left operand: " ^ msg));
        (try sub_type t2 TBool (* Types.TBool -> TBool *)
         with Failure msg ->
           failwith ("Type error in Or (||) right operand: " ^ msg));
        (TBool, TAExprNode (Or ((t1, a1), (t2, a2))))
    | Not e1 ->
        let t1, a1 = aux env e1 in
        (try sub_type t1 TBool (* Types.TBool -> TBool *)
         with Failure msg -> failwith ("Type error in Not operand: " ^ msg));
        (TBool, TAExprNode (Not (t1, a1)))
    | Observe e1 ->
        let t1, a1 = aux env e1 in
        (try sub_type t1 TBool (* Argument must be TBool *)
         with Failure msg ->
           failwith ("Type error in Observe argument: " ^ msg));
        (TUnit, TAExprNode (Observe (t1, a1)))
        (* Result is TUnit *)
    | Fix (f, x, e_body) ->
        let fun_type_itself = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (* Type of f *)
        let param_type = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (* Type of x *)
        let env_body =
          StringMap.add x param_type (StringMap.add f fun_type_itself env)
        in
        let body_texpr = aux env_body e_body in
        let body_ret_type = fst body_texpr in
        let actual_fun_type = TFun (param_type, body_ret_type) in (* Types.TFun -> TFun *)
        unify fun_type_itself actual_fun_type;
        (fun_type_itself, TAExprNode (Fix (f, x, body_texpr)))
    | Nil ->
        let elem_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (TList elem_ty, TAExprNode Nil)
    | Cons (e_hd, e_tl) ->
        let t_hd, a_hd = aux env e_hd in
        let t_tl, a_tl = aux env e_tl in
        (try unify t_tl (TList t_hd)
         with Failure msg ->
           failwith ("Type error in list construction (::): " ^ msg));
        (t_tl, TAExprNode (Cons ((t_hd, a_hd), (t_tl, a_tl))))
    | MatchList (e_match, e_nil, y, ys, e_cons) ->
        let t_match, a_match = aux env e_match in
        let elem_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (try unify t_match (TList elem_ty)
         with Failure msg ->
           failwith
             ("Type error in match expression (expected list type): " ^ msg));
        (* Type check nil branch *)
        let t_nil, a_nil = aux env e_nil in
        (* Type check cons branch *)
        let env_cons = StringMap.add y elem_ty (StringMap.add ys t_match env) in
        let t_cons, a_cons = aux env_cons e_cons in
        (* Unify branch types *)
        let result_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (try
           sub_type t_nil result_ty;
           sub_type t_cons result_ty
         with Failure msg -> failwith ("Type error in match branches: " ^ msg));
        ( result_ty,
          TAExprNode
            (MatchList
               ((t_match, a_match), (t_nil, a_nil), y, ys, (t_cons, a_cons))) )
    | Ref e1 ->
        let t1, a1 = aux env e1 in
        (TRef t1, TAExprNode (Ref (t1, a1)))
    | Deref e1 ->
        let t1, a1 = aux env e1 in
        let val_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (try unify t1 (TRef val_ty)
         with Failure msg -> failwith ("Type error in dereference (!): " ^ msg));
        (force val_ty, TAExprNode (Deref (t1, a1))) (* Types.force -> force *)
    | Assign (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let val_ty = fresh_meta () in (* Types.fresh_meta -> fresh_meta *)
        (try
           unify t1 (TRef val_ty);
           sub_type t2 (force val_ty) (* Types.force -> force *)
         with Failure msg -> failwith ("Type error in assignment (:=): " ^ msg));
        (TUnit, TAExprNode (Assign ((t1, a1), (t2, a2))))
    | Seq (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        (t2, TAExprNode (Seq ((t1, a1), (t2, a2))))
        (* Type of sequence is type of e2 *)
    | Unit -> (TUnit, TAExprNode Unit) (* Types.TUnit -> TUnit *)
    | RuntimeError s -> (fresh_meta (), TAExprNode (RuntimeError s)) (* Types.fresh_meta -> fresh_meta *)
  in

  aux StringMap.empty e

(**
 * @function elab_bool
 * @brief Elaborates an expression and asserts that its type is `TBool`.
 * Convenience wrapper around `elab` for conditions or boolean expressions.
 *
 * @param e The input expression (`Ast.expr`).
 * @return The elaborated typed expression (`TypedAst.texpr`).
 * @raise Failure if the expression does not type-check to `TBool`.
 *)
let elab_bool (e : expr) : texpr =
  let t, a = elab e in
  sub_type t TBool; (* Assert type is TBool *)
  (t, a)

(**
 * @function prob_cdistr_interval
 * @brief Calculates the probability mass of a continuous distribution `dist`
 * within the interval `[left, right)`.
 * Uses `P(X in [left, right)) = CDF(right) - CDF(left)`.
 *
 * @param left The left endpoint of the interval.
 * @param right The right endpoint of the interval.
 * @param dist The `Distributions.cdistr` instance.
 * @return The probability (float).
 *)
let prob_cdistr_interval (left : float) (right : float)
    (dist : Distributions.cdistr) : float =
  let cdf = Distributions.cdistr_cdf dist in
  cdf right -. cdf left

(* ======== Discretization: TAST to AST ======== *)

(**
 * @function discretize
 * @brief Converts a typed ContDice expression (TAST) into a potentially discretized
 * untyped expression (AST).
 *
 * The core idea of discretization:
 * 1. Type elaboration (`elab`) infers `BoundBag`s for `TFloat` types. These bags
 *    collect all float constants that a float-typed expression is compared against
 *    (e.g., in `x < 5.0`, `5.0` is a bound for `x`).
 * 2. `discretize` uses these inferred bounds (now called "cuts") to transform
 *    continuous operations:
 *    - `Sample` from a continuous distribution becomes a `DistrCase` (discrete distribution)
 *      over intervals defined by the cuts. The probability of each interval is calculated
 *      using the original distribution's CDF.
 *    - Comparisons like `x < y` (where `x`, `y` are floats) become finite set comparisons
 *      (e.g., `FinLt`) operating on discrete indices representing the intervals.
 *    - Float constants themselves are mapped to their corresponding discrete interval index.
 *
 * If a `TFloat`'s `BoundBag` is `Top` (no finite set of comparison points was inferred),
 * the parts of the expression involving that float are typically not discretized and
 * retain their original continuous form.
 *
 * @param e The input typed expression (`TypedAst.texpr`).
 * @return The (potentially) discretized `Ast.expr`.
 *)
let discretize (e : texpr) : expr =
  (**
   * @function aux
   * @brief Recursive helper for `discretize`.
   * @param texpr_node The current typed AST node `(ty, TAExprNode ae_node)` to discretize.
   * @return The discretized `Ast.expr`.
   *)
  let rec aux ((ty, TAExprNode ae_node) : texpr) : expr =
    match ae_node with
    | Const f -> ( (* Discretize a float constant *)
        let bounds_bag_ref =
          match force ty with (* Get the type, forcing meta variables *)
          | TFloat (b, _) -> b (* Extract the BoundBag from TFloat *)
          | _ -> failwith "Type error during discretize: Const expects TFloat"
        in
        match Bags.BoundBag.get bounds_bag_ref with
        | Bags.Top -> ExprNode (Const f) (* If no bounds (Top), keep as original float constant *)
        | Bags.Finite bound_set_from_context ->
            let idx, modulus =
              get_const_idx_and_modulus f bound_set_from_context
            in
            if idx = 0 then (
              (if modulus = 1 then
                 let sz = Util.bit_length (int_of_float f) in
                 if sz > !Util.curr_max_int_sz then Util.curr_max_int_sz := sz);
              (* Discrete leaf *)
              ExprNode (FinConst (int_of_float f, modulus))
              (* Keep original const if idx is zero *))
            else ExprNode (FinConst (idx, modulus)))
    | BoolConst b -> ExprNode (BoolConst b)
    | Var x -> ExprNode (Var x)
    | Let (x, te1, te2) -> ExprNode (Let (x, aux te1, aux te2))
    | Sample dist_exp -> (
        let outer_sample_ty = ty in
        (* Type of the Sample expression itself *)
        let bounds_bag_of_outer_sample =
          match force outer_sample_ty with (* Types.force -> force *)
          | TFloat (b, _) -> b (* Types.TFloat -> TFloat *)
          | _ ->
              failwith
                "Internal error: Sample expression's type is not TFloat during \
                 discretize"
        in
        let set_or_top_val = Bags.BoundBag.get bounds_bag_of_outer_sample in

        match set_or_top_val with
        | Bags.Top -> (
            (* If outer Sample's bounds are Top, fallback to simple recursive
               discretization of params *)
            match dist_exp with
            | Distr1 (kind, texpr_arg) ->
                let texpr_arg_discretized = aux texpr_arg in
                ExprNode (Sample (Distr1 (kind, texpr_arg_discretized)))
            | Distr2 (kind, texpr_arg1, texpr_arg2) ->
                let texpr_arg1_discretized = aux texpr_arg1 in
                let texpr_arg2_discretized = aux texpr_arg2 in
                ExprNode
                  (Sample
                     (Distr2
                        (kind, texpr_arg1_discretized, texpr_arg2_discretized)))
            )
        | Bags.Finite outer_bound_set -> (
            (* Outer Sample's bounds are Finite, proceed with interval-based
               discretization *)
            let outer_cuts_as_bounds = Bags.BoundSet.elements outer_bound_set in
            let overall_modulus = 1 + List.length outer_cuts_as_bounds in
            if overall_modulus <= 0 then
              failwith "Internal error: Sample modulus must be positive";

            let final_expr_producer (concrete_distr : Distributions.cdistr) :
                expr =
              let get_float_val_from_bound (b : Bags.bound) : float =
                match b with Bags.Less f -> f | Bags.LessEq f -> f
              in

              let intervals_for_probs =
                List.init overall_modulus (fun k_idx ->
                    (* k_idx is the discrete outcome index, from 0 to
                       overall_modulus - 1 *)
                    let left_for_cdf =
                      if k_idx = 0 then neg_infinity
                      else
                        get_float_val_from_bound
                          (List.nth outer_cuts_as_bounds (k_idx - 1))
                    in
                    let right_for_cdf =
                      if k_idx = overall_modulus - 1 then infinity
                      else
                        get_float_val_from_bound
                          (List.nth outer_cuts_as_bounds k_idx)
                    in
                    (* Ensure left <= right. If bounds imply same float val,
                       interval is [v,v] -> prob should be 0 by CDF diff *)
                    ( min left_for_cdf right_for_cdf,
                      max left_for_cdf right_for_cdf ))
              in
              let probs =
                List.map
                  (fun (l, r) -> prob_cdistr_interval l r concrete_distr)
                  intervals_for_probs
              in
              if List.exists (fun p -> p < -0.0001 || p > 1.0001) probs then
                failwith
                  ("Internal error: generated probabilities are invalid: "
                  ^ Pretty.string_of_float_list probs
                  ^ " for distribution "
                  ^ Pretty.string_of_cdistr concrete_distr
                  ^ Printf.sprintf " with %d outer bounds."
                      (List.length outer_cuts_as_bounds));
              let sum_probs = List.fold_left ( +. ) 0.0 probs in
              if abs_float (sum_probs -. 1.0) > 0.001 then ();

              let distr_cases =
                List.mapi
                  (fun i prob ->
                    ( ExprNode (FinConst (i, overall_modulus)),
                      max 0.0 (min 1.0 prob) ))
                  probs
              in
              ExprNode (DistrCase distr_cases)
            in

            let default_branch_expr =
              match dist_exp with
              | Distr1 (kind, texpr_arg) ->
                  ExprNode (Sample (Distr1 (kind, aux texpr_arg)))
              | Distr2 (kind, texpr_arg1, texpr_arg2) ->
                  ExprNode
                    (Sample (Distr2 (kind, aux texpr_arg1, aux texpr_arg2)))
            in

            let get_possible_floats_from_param (param_texpr : texpr) :
                float list option =
              let param_ty, _ = param_texpr in
              match force param_ty with (* Types.force -> force *)
              | TFloat (_, consts_bag_ref) -> ( (* Types.TFloat -> TFloat *)
                  match Bags.FloatBag.get consts_bag_ref with
                  | Bags.Finite float_set ->
                      if Bags.FloatSet.is_empty float_set then None
                      else
                        Some
                          (Bags.FloatSet.elements float_set
                          |> List.sort_uniq compare)
                  | Bags.Top -> None)
              | _ -> None
            in

            let get_param_modulus_and_cuts (param_texpr : texpr) :
                (int * Bags.bound list) option =
              let param_ty, _ = param_texpr in
              match force param_ty with (* Types.force -> force *)
              | TFloat (b_bag, _) -> ( (* Types.TFloat -> TFloat *)
                  match Bags.BoundBag.get b_bag with
                  | Top -> None
                  | Finite bs_set ->
                      let cuts = Bags.BoundSet.elements bs_set in
                      let modulus = 1 + List.length cuts in
                      if modulus <= 0 then None else Some (modulus, cuts))
              | _ -> None
            in

            let rec build_nested_ifs (val_var_name : string)
                (param_modulus : int) (arms : (expr * expr) list)
                (default_expr_if_all_fail : expr) : expr =
              match arms with
              | [] ->
                  (* This case should ideally not be reached if
                     `generate_runtime_match_for_param` ensures `arms` (derived
                     from `possible_floats`) is non-empty when calling this. If
                     `possible_floats` is empty, `default_expr_if_all_fail` is
                     returned directly by the caller. If it *is* reached, it
                     means something unexpected happened or initial arms list
                     was empty. *)
                  failwith
                    "build_nested_ifs: Reached empty arms list, this should be \
                     handled by caller or indicates an issue."
              | [ (_target_finconst_expr, body_expr) ] ->
                  (* This is the last arm. If all previous conditions were
                     false, this one is effectively the 'else'. The assumption
                     is that val_var_name *must* match one of the targets if
                     possible_floats was exhaustive. So, the condition
                     FinEq(ExprNode (Var val_var_name), target_finconst_expr,
                     param_modulus) is implicitly true here. *)
                  body_expr
              | (target_finconst_expr, body_expr) :: rest_arms ->
                  let current_val_expr = ExprNode (Var val_var_name) in
                  let condition =
                    ExprNode
                      (FinEq
                         (current_val_expr, target_finconst_expr, param_modulus))
                  in
                  ExprNode
                    (If
                       ( condition,
                         body_expr,
                         build_nested_ifs val_var_name param_modulus rest_arms
                           default_expr_if_all_fail ))
            in

            let generate_runtime_match_for_param
                ?(already_discretized_expr : expr option = None)
                (param_texpr : texpr) (param_name_str : string)
                (build_body_fn : float -> expr)
                (default_expr_for_this_param : expr) : expr =
              match
                ( get_possible_floats_from_param param_texpr,
                  get_param_modulus_and_cuts param_texpr )
              with
              | ( Some possible_floats,
                  Some (param_modulus, param_actual_bound_cuts_as_bounds) ) ->
                  if List.length possible_floats = 0 then
                    default_expr_for_this_param
                  else if List.length possible_floats = 1 then
                    build_body_fn (List.hd possible_floats)
                  else
                    let actual_discretized_param_expr =
                      match already_discretized_expr with
                      | Some ade -> ade
                      | None -> aux param_texpr
                    in
                    let match_arms =
                      List.map
                        (fun f_val ->
                          let param_consts_bag_for_f =
                            Bags.FloatBag.create
                              (Finite (FloatSet.singleton f_val))
                          in
                          let param_bounds_bag_for_f =
                            Bags.BoundBag.create
                              (Finite
                                 (Bags.BoundSet.of_list
                                    param_actual_bound_cuts_as_bounds))
                          in
                          let texpr_const_f_val =
                            ( TFloat
                                (param_bounds_bag_for_f, param_consts_bag_for_f),
                              TAExprNode (Const f_val) )
                          in
                          let target_finconst_expr = aux texpr_const_f_val in
                          let body = build_body_fn f_val in
                          (target_finconst_expr, body))
                        possible_floats
                    in

                    Util.gen_let ("_disc_" ^ param_name_str)
                      actual_discretized_param_expr (fun let_var_name ->
                        build_nested_ifs let_var_name param_modulus match_arms
                          default_expr_for_this_param)
              | _ -> default_expr_for_this_param
            in

            match dist_exp with
            | Distr1 (DExponential, texpr_lambda) ->
                generate_runtime_match_for_param texpr_lambda "lambda"
                  (fun val_lambda ->
                    if val_lambda <= 0.0 then
                      ExprNode
                        (RuntimeError "Exponential lambda must be positive")
                    else
                      final_expr_producer (Distributions.Exponential val_lambda))
                  default_branch_expr
            | Distr1 (DLaplace, texpr_scale) ->
                generate_runtime_match_for_param texpr_scale "scale"
                  (fun val_scale ->
                    if val_scale <= 0.0 then
                      ExprNode (RuntimeError "Laplace scale must be positive")
                    else final_expr_producer (Distributions.Laplace val_scale))
                  default_branch_expr
            | Distr1 (DCauchy, texpr_scale) ->
                generate_runtime_match_for_param texpr_scale "scale"
                  (fun val_scale ->
                    if val_scale <= 0.0 then
                      ExprNode (RuntimeError "Cauchy scale must be positive")
                    else final_expr_producer (Distributions.Cauchy val_scale))
                  default_branch_expr
            | Distr1 (DTDist, texpr_nu) ->
                generate_runtime_match_for_param texpr_nu "nu"
                  (fun val_nu ->
                    if val_nu <= 0.0 then
                      ExprNode (RuntimeError "TDist nu must be positive")
                    else final_expr_producer (Distributions.TDist val_nu))
                  default_branch_expr
            | Distr1 (DChi2, texpr_nu) ->
                generate_runtime_match_for_param texpr_nu "nu"
                  (fun val_nu ->
                    if val_nu <= 0.0 then
                      ExprNode (RuntimeError "Chi2 nu must be positive")
                    else final_expr_producer (Distributions.Chi2 val_nu))
                  default_branch_expr
            | Distr1 (DLogistic, texpr_scale) ->
                generate_runtime_match_for_param texpr_scale "scale"
                  (fun val_scale ->
                    if val_scale <= 0.0 then
                      ExprNode (RuntimeError "Logistic scale must be positive")
                    else final_expr_producer (Distributions.Logistic val_scale))
                  default_branch_expr
            | Distr1 (DRayleigh, texpr_sigma) ->
                generate_runtime_match_for_param texpr_sigma "sigma"
                  (fun val_sigma ->
                    if val_sigma <= 0.0 then
                      ExprNode (RuntimeError "Rayleigh sigma must be positive")
                    else final_expr_producer (Distributions.Rayleigh val_sigma))
                  default_branch_expr
            | Distr2 (DUniform, texpr_a, texpr_b) -> (
                let discretized_b_expr = aux texpr_b in
                let core_logic (eff_discretized_b_expr : expr) =
                  generate_runtime_match_for_param texpr_a "a"
                    (fun val_a ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:(Some eff_discretized_b_expr)
                        texpr_b "b"
                        (fun val_b ->
                          if val_a > val_b then
                            ExprNode (RuntimeError "Uniform low > high")
                          else
                            final_expr_producer
                              (Distributions.Uniform (val_a, val_b)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_b_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_b_expr
                | _ ->
                    Util.gen_let "_h_b" discretized_b_expr (fun hoisted_b_var ->
                        core_logic (ExprNode (Var hoisted_b_var))))
            | Distr2 (DGaussian, texpr_mu, texpr_sigma) -> (
                let discretized_sigma_expr = aux texpr_sigma in
                let core_logic (eff_discretized_sigma_expr : expr) =
                  generate_runtime_match_for_param texpr_mu "mu"
                    (fun val_mu ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:
                          (Some eff_discretized_sigma_expr) texpr_sigma "sigma"
                        (fun val_sigma ->
                          if val_sigma <= 0.0 then
                            ExprNode
                              (RuntimeError "Gaussian sigma must be positive")
                          else
                            final_expr_producer
                              (Distributions.Gaussian (val_mu, val_sigma)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_sigma_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_sigma_expr
                | _ ->
                    Util.gen_let "_h_sigma" discretized_sigma_expr
                      (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))
            | Distr2 (DBeta, texpr_alpha, texpr_beta_param) -> (
                let discretized_beta_param_expr = aux texpr_beta_param in
                let core_logic (eff_discretized_beta_param_expr : expr) =
                  generate_runtime_match_for_param texpr_alpha "alpha"
                    (fun val_alpha ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:
                          (Some eff_discretized_beta_param_expr)
                        texpr_beta_param "beta_param"
                        (fun val_beta_param ->
                          if val_alpha <= 0.0 || val_beta_param <= 0.0 then
                            ExprNode
                              (RuntimeError
                                 "Beta alpha and beta_param must be positive")
                          else
                            final_expr_producer
                              (Distributions.Beta (val_alpha, val_beta_param)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_beta_param_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_beta_param_expr
                | _ ->
                    Util.gen_let "_h_beta_p" discretized_beta_param_expr
                      (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))
            | Distr2 (DLogNormal, texpr_mu, texpr_sigma) -> (
                let discretized_sigma_expr = aux texpr_sigma in
                let core_logic (eff_discretized_sigma_expr : expr) =
                  generate_runtime_match_for_param texpr_mu "mu"
                    (fun val_mu ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:
                          (Some eff_discretized_sigma_expr) texpr_sigma "sigma"
                        (fun val_sigma ->
                          if val_sigma <= 0.0 then
                            ExprNode
                              (RuntimeError "LogNormal sigma must be positive")
                          else
                            final_expr_producer
                              (Distributions.LogNormal (val_mu, val_sigma)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_sigma_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_sigma_expr
                | _ ->
                    Util.gen_let "_h_sigma_ln" discretized_sigma_expr
                      (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))
            | Distr2 (DGamma, texpr_shape, texpr_scale) -> (
                let discretized_scale_expr = aux texpr_scale in
                let core_logic (eff_discretized_scale_expr : expr) =
                  generate_runtime_match_for_param texpr_shape "shape"
                    (fun val_shape ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:
                          (Some eff_discretized_scale_expr) texpr_scale "scale"
                        (fun val_scale ->
                          if val_shape <= 0.0 || val_scale <= 0.0 then
                            ExprNode
                              (RuntimeError
                                 "Gamma shape and scale must be positive")
                          else
                            final_expr_producer
                              (Distributions.Gamma (val_shape, val_scale)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_scale_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_scale_expr
                | _ ->
                    Util.gen_let "_h_scale_g" discretized_scale_expr
                      (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))
            | Distr2 (DPareto, texpr_a, texpr_b) -> (
                let discretized_b_expr = aux texpr_b in
                let core_logic (eff_discretized_b_expr : expr) =
                  generate_runtime_match_for_param texpr_a "a"
                    (fun val_a ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:(Some eff_discretized_b_expr)
                        texpr_b "b"
                        (fun val_b ->
                          if val_a <= 0.0 || val_b <= 0.0 then
                            ExprNode
                              (RuntimeError "Pareto a and b must be positive")
                          else
                            final_expr_producer
                              (Distributions.Pareto (val_a, val_b)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_b_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_b_expr
                | _ ->
                    Util.gen_let "_h_b_p" discretized_b_expr (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))
            | Distr2 (DWeibull, texpr_a, texpr_b) -> (
                let discretized_b_expr = aux texpr_b in
                let core_logic (eff_discretized_b_expr : expr) =
                  generate_runtime_match_for_param texpr_a "a"
                    (fun val_a ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:(Some eff_discretized_b_expr)
                        texpr_b "b"
                        (fun val_b ->
                          if val_a <= 0.0 || val_b <= 0.0 then
                            ExprNode
                              (RuntimeError "Weibull a and b must be positive")
                          else
                            final_expr_producer
                              (Distributions.Weibull (val_a, val_b)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_b_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_b_expr
                | _ ->
                    Util.gen_let "_h_b_w" discretized_b_expr (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))
            | Distr2 (DGumbel1, texpr_a, texpr_b) -> (
                let discretized_b_expr = aux texpr_b in
                let core_logic (eff_discretized_b_expr : expr) =
                  generate_runtime_match_for_param texpr_a "a"
                    (fun val_a ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:(Some eff_discretized_b_expr)
                        texpr_b "b"
                        (fun val_b ->
                          if val_b <= 0.0 then
                            (* Gumbel1 constraint is on b *)
                            ExprNode (RuntimeError "Gumbel1 b must be positive")
                          else
                            final_expr_producer
                              (Distributions.Gumbel1 (val_a, val_b)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_b_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_b_expr
                | _ ->
                    Util.gen_let "_h_b_g1" discretized_b_expr
                      (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))
            | Distr2 (DGumbel2, texpr_a, texpr_b) -> (
                let discretized_b_expr = aux texpr_b in
                let core_logic (eff_discretized_b_expr : expr) =
                  generate_runtime_match_for_param texpr_a "a"
                    (fun val_a ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:(Some eff_discretized_b_expr)
                        texpr_b "b"
                        (fun val_b ->
                          if val_b <= 0.0 then
                            (* Gumbel2 constraint is on b *)
                            ExprNode (RuntimeError "Gumbel2 b must be positive")
                          else
                            final_expr_producer
                              (Distributions.Gumbel2 (val_a, val_b)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_b_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_b_expr
                | _ ->
                    Util.gen_let "_h_b_g2" discretized_b_expr
                      (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))
            | Distr2 (DExppow, texpr_a, texpr_b) -> (
                let discretized_b_expr = aux texpr_b in
                let core_logic (eff_discretized_b_expr : expr) =
                  generate_runtime_match_for_param texpr_a "a"
                    (fun val_a ->
                      generate_runtime_match_for_param
                        ~already_discretized_expr:(Some eff_discretized_b_expr)
                        texpr_b "b"
                        (fun val_b ->
                          if val_a <= 0.0 || val_b <= 0.0 then
                            ExprNode
                              (RuntimeError "Exppow a and b must be positive")
                          else
                            final_expr_producer
                              (Distributions.Exppow (val_a, val_b)))
                        default_branch_expr)
                    default_branch_expr
                in
                match discretized_b_expr with
                | ExprNode (Var _)
                | ExprNode (Const _)
                | ExprNode (BoolConst _)
                | ExprNode (FinConst _) ->
                    core_logic discretized_b_expr
                | _ ->
                    Util.gen_let "_h_b_ep" discretized_b_expr
                      (fun hoisted_var ->
                        core_logic (ExprNode (Var hoisted_var))))))
    | DistrCase cases ->
        (* Recursively discretize the expressions within the cases *)
        let discretized_cases =
          List.map (fun (texpr, prob) -> (aux texpr, prob)) cases
        in
        ExprNode (DistrCase discretized_cases)
    | Less (te1, te2) -> (
        let t1 = fst te1 in
        let t2 = fst te2 in
        let b1 =
          match force t1 with (* Types.force -> force *)
          | TFloat (b, _) -> b (* Types.TFloat -> TFloat *)
          | _ -> failwith "Type error: Less expects float"
        in
        let b2 =
          match force t2 with (* Types.force -> force *)
          | TFloat (b, _) -> b (* Types.TFloat -> TFloat *)
          | _ -> failwith "Type error: Less expects float on right operand"
        in
        let val1 = Bags.BoundBag.get b1 in
        let val2 = Bags.BoundBag.get b2 in
        if not (Bags.BoundSetContents.equal val1 val2) then
          failwith
            "Internal error: Less operands have different bound bag values \
             despite elaboration";

        match val1 with
        | Bags.Top -> ExprNode (Less (aux te1, aux te2))
        | Bags.Finite bound_set ->
            let n = 1 + List.length (Bags.BoundSet.elements bound_set) in
            let d1 = aux te1 in
            let d2 = aux te2 in
            ExprNode (FinLt (d1, d2, n)))
    | LessEq (te1, te2) -> (
        let t1 = fst te1 in
        let t2 = fst te2 in
        let b1 =
          match force t1 with (* Types.force -> force *)
          | TFloat (b, _) -> b (* Types.TFloat -> TFloat *)
          | _ -> failwith "Type error: LessEq expects float on left operand"
        in
        let b2 =
          match force t2 with (* Types.force -> force *)
          | TFloat (b, _) -> b (* Types.TFloat -> TFloat *)
          | _ -> failwith "Type error: LessEq expects float on right operand"
        in
        let val1 = Bags.BoundBag.get b1 in
        let val2 = Bags.BoundBag.get b2 in
        if not (Bags.BoundSetContents.equal val1 val2) then
          failwith
            "Internal error: LessEq operands have different bound bag values \
             despite elaboration";

        match val1 with
        | Bags.Top -> ExprNode (LessEq (aux te1, aux te2))
        | Bags.Finite bound_set ->
            let n = 1 + List.length (Bags.BoundSet.elements bound_set) in
            let d1 = aux te1 in
            let d2 = aux te2 in
            ExprNode (FinLeq (d1, d2, n)))
    | If (te1, te2, te3) -> ExprNode (If (aux te1, aux te2, aux te3))
    | Pair (te1, te2) -> ExprNode (Pair (aux te1, aux te2))
    | First te -> ExprNode (First (aux te))
    | Second te -> ExprNode (Second (aux te))
    | Fun (x, te) -> ExprNode (Fun (x, aux te))
    | FuncApp (te1, te2) -> ExprNode (FuncApp (aux te1, aux te2))
    | LoopApp (te1, te2, te3) -> ExprNode (LoopApp (aux te1, aux te2, te3))
    | FinConst (k, n) -> ExprNode (FinConst (k, n))
    | FinLt (te1, te2, n) -> ExprNode (FinLt (aux te1, aux te2, n))
    | FinLeq (te1, te2, n) -> ExprNode (FinLeq (aux te1, aux te2, n))
    | FinEq (te1, te2, n) -> ExprNode (FinEq (aux te1, aux te2, n))
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

let discretize_top (e : texpr) : expr = discretize e
