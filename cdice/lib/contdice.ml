(* Main implementation of continuous dice *)

open Types
open Bags (* Open Bags to access FloatSet, BoundSet, FloatBag, BoundBag etc. *)
(* FloatBag is now defined in Bags module *)

(* Re-export internal modules needed by executable/tests *)
module Parse = Parse 
module Pretty = Pretty
module Util = Util
module To_dice = To_dice
module Interp = Interp
module Types = Types (* Explicitly alias Types here *)
module Distributions = Distributions (* Explicitly alias Distributions here *)
module Bags = Bags   (* And Bags *)

module StringMap = Map.Make(String)

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

(* ======== Types, Subtyping, and Unification ======== *)

(* Enforce t_sub is a subtype of t_super *)
let rec sub_type (t_sub : ty) (t_super : ty) : unit =
  match Types.force t_sub, Types.force t_super with
  (* Base Cases *)
  | TBool,    TBool      -> ()
  | TFin n1, TFin n2 when n1 = n2 -> () 
  | TUnit, TUnit -> ()
  (* Structural Cases *)
  | TFloat (b1, c1), TFloat (b2, c2) -> 
      Bags.BoundBag.eq b1 b2;  (* Bounds must be consistent *) 
      Bags.FloatBag.leq c1 c2  (* Constants flow sub -> super *) 
  | TPair(a1, b1), TPair(a2, b2) -> 
      sub_type a1 a2; (* Covariant *) 
      sub_type b1 b2  (* Covariant *) 
  | TFun(a1, b1), TFun(a2, b2) -> 
      sub_type a2 a1; (* Contravariant argument *) 
      sub_type b1 b2  (* Covariant result *) 
  | TList t1, TList t2 -> sub_type t1 t2 (* Covariant *) 
  | TRef t1, TRef t2 -> unify t1 t2 (* Invariant *)
  | TMeta r, _ ->
    (match Types.force t_super with (* Ensure t_super is forced *)
    | TMeta r' -> (Types.listen r (fun t -> sub_type t t_super); Types.listen r' (fun t' -> sub_type t_sub t'))
    | TBool -> Types.assign r TBool
    | TFin n -> Types.assign r (TFin n)
    | TPair (_, _) -> let a_meta = Types.fresh_meta () in let b_meta = Types.fresh_meta () in
      Types.assign r (TPair (a_meta, b_meta)); sub_type t_sub t_super
    | TFun (_, _) -> let a_meta = Types.fresh_meta () in let b_meta = Types.fresh_meta () in
      Types.assign r (TFun (a_meta, b_meta)); sub_type t_sub t_super
    | TFloat (_, _) -> let b_bag = Bags.fresh_bound_bag () in let c_bag = Bags.fresh_float_bag () in
      Types.assign r (TFloat (b_bag, c_bag)); sub_type t_sub t_super
    | TUnit -> Types.assign r TUnit (* Handle TUnit for t_super *)
    | TList _ -> let elem_meta = Types.fresh_meta () in 
                 Types.assign r (TList elem_meta); sub_type t_sub t_super
    | TRef _ -> let ref_meta = Types.fresh_meta () in
                Types.assign r (TRef ref_meta); sub_type t_sub t_super
    )
  | _, TMeta r ->
    (match Types.force t_sub with (* Ensure t_sub is forced *)
    | TMeta r' -> (Types.listen r (fun t -> sub_type t_sub t); Types.listen r' (fun t' -> sub_type t_sub t'))
    | TBool -> Types.assign r TBool
    | TFin n -> Types.assign r (TFin n)
    | TPair (_, _) -> let a_meta = Types.fresh_meta () in let b_meta = Types.fresh_meta () in
      Types.assign r (TPair (a_meta, b_meta)); sub_type t_sub t_super
    | TFun (_, _) -> let a_meta = Types.fresh_meta () in let b_meta = Types.fresh_meta () in
      Types.assign r (TFun (a_meta, b_meta)); sub_type t_sub t_super
    | TFloat (_, _) -> let b_bag = Bags.fresh_bound_bag () in let c_bag = Bags.fresh_float_bag () in
      Types.assign r (TFloat (b_bag, c_bag)); sub_type t_sub t_super
    | TUnit -> Types.assign r TUnit (* Handle TUnit for t_sub *)
    | TList _ -> let elem_meta = Types.fresh_meta () in 
                 Types.assign r (TList elem_meta); sub_type t_sub t_super
    | TRef _ -> let ref_meta = Types.fresh_meta () in
                Types.assign r (TRef ref_meta); sub_type t_sub t_super
    )
  (* Error Case *) 
  | _, _ -> 
      let msg = Printf.sprintf "Type mismatch: cannot subtype %s <: %s"
        (Pretty.string_of_ty t_sub) (Pretty.string_of_ty t_super)
      in
      failwith msg

(* Unification: enforce t1 = t2 by bidirectional subtyping *) 
and unify (t1 : ty) (t2 : ty) : unit =
  try 
    sub_type t1 t2; 
    sub_type t2 t1
  with Failure msg -> 
    (* Provide a unification-specific error message *)
    let unified_msg = Printf.sprintf "Type mismatch: cannot unify %s and %s\n(Subtyping error: %s)"
      (Pretty.string_of_ty t1) (Pretty.string_of_ty t2) msg
    in
    failwith unified_msg

(* ======== Annotated expressions ======== *)

(* Elaborator: expr -> texpr, generating bag constraints and performing type checking *)
let elab (e : expr) : texpr =
  let rec aux (env : ty StringMap.t) (ExprNode e_node : expr) : texpr =
    match e_node with
    | Const f -> 
      (* Constant float: Create bag refs *) 
      let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in 
      let consts_bag_ref = Bags.FloatBag.create (Finite (FloatSet.singleton f)) in
      (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (Const f))
    | BoolConst b ->
      (TBool, TAExprNode (BoolConst b))
    | Var x ->
      (try 
        let ty = StringMap.find x env in
        (ty, TAExprNode (Var x))
       with Not_found -> 
        failwith ("Unbound variable: " ^ x))

    | Let (x, e1, e2) ->
      let t1, a1 = aux env e1 in
      let env' = StringMap.add x t1 env in
      let t2, a2 = aux env' e2 in
      (t2, TAExprNode (Let (x, (t1,a1), (t2,a2))))

    | Sample dist_exp ->
      let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in 
      let consts_bag_ref = Bags.FloatBag.create Top in 

      (* Helper to propagate float constants from an argument to its own bound bag *)
      let add_floats_to_boundbag (float_bag : FloatBag.bag) (bound_bag : BoundBag.bag) =
        let listener () =
          let v = Bags.FloatBag.get float_bag in
          (match v with
          | Finite s -> Bags.BoundBag.add_all s bound_bag
            | Top -> Bags.BoundBag.leq (Bags.BoundBag.create Top) bound_bag)
        in
        Bags.FloatBag.listen float_bag listener
      in

      (* Helper to make the output distribution's bound bag Top if any input's bound bag becomes Top *)
      let make_output_top_if_input_boundbag_is_top input_bound_bag =
        let listener () =
          let v = Bags.BoundBag.get input_bound_bag in
          (match v with
          | Top -> Bags.BoundBag.leq (Bags.BoundBag.create Top) bounds_bag_ref (* bounds_bag_ref is from Sample scope *)
          | _ -> ())
        in
        Bags.BoundBag.listen input_bound_bag listener
      in

      (* Helper to make an input's bound bag Top if the output distribution's bound bag becomes Top *)
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
          let t_arg_bound_bag = Bags.fresh_bound_bag () in
          let t_arg_float_bag = Bags.fresh_float_bag () in
          (try unify t_arg (Types.TFloat (t_arg_bound_bag, t_arg_float_bag))
           with Failure msg -> 
            let kind_str = Pretty.string_of_expr_indented (ExprNode (Sample (Distr1 (dist_kind, arg_e)))) in (* Get a string for the kind *)
            failwith (Printf.sprintf "Type error in Sample (%s) argument: %s" kind_str msg));
          
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
      (* Check probabilities sum to 1 *) 
      let probs = List.map snd cases in
      let sum = List.fold_left (+.) 0.0 probs in
      if abs_float (sum -. 1.0) > 0.0001 then
        failwith (Printf.sprintf "DistrCase probabilities must sum to 1.0, got %f" sum);
      
      (* Type-check all expressions and subtype them into a fresh result type *) 
      let typed_cases = List.map (fun (e, p) -> (aux env e, p)) cases in
      let result_ty = Types.fresh_meta () in (* Fresh meta for the result *) 
      List.iter (fun ((branch_ty, _), _) -> 
        try sub_type branch_ty result_ty (* Enforce branch <: result *)
        with Failure msg -> failwith ("Type error in DistrCase branches: " ^ msg)
      ) typed_cases;
      
      let annotated_cases = List.map (fun (texpr, prob) -> (texpr, prob)) typed_cases in
      (result_ty, TAExprNode (DistrCase annotated_cases))

    | Less (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let b_meta = Bags.fresh_bound_bag () in (* Shared bound bag for unification *)
        let c_meta1 = Bags.fresh_float_bag () in
        let c_meta2 = Bags.fresh_float_bag () in
        (try unify t1 (Types.TFloat (b_meta, c_meta1)) (* Unify t1 with TFloat(b_meta, c1) *)
         with Failure msg -> failwith (Printf.sprintf "Type error in Less (<) left operand: %s" msg));
        (try unify t2 (Types.TFloat (b_meta, c_meta2)) (* Unify t2 with TFloat(b_meta, c2) *)
         with Failure msg -> failwith (Printf.sprintf "Type error in Less (<) right operand: %s" msg));

        (* Nested listener logic for Less *) 
        let listener () = (* Listener takes unit *)
          let v1 = Bags.FloatBag.get c_meta1 in (* Get value inside listener *)
          let v2 = Bags.FloatBag.get c_meta2 in (* Get value inside listener *)
          match v1, v2 with
          | Top, Top ->
              (* Both Top -> BoundBag should be Top *) 
              Bags.BoundBag.leq (Bags.BoundBag.create Top) b_meta
          | _, _ ->
              (* At least one is not Top. Add bounds from Finite bags. *)
              (* Temporarily store bounds to add *) 
              let bounds_to_add = ref Bags.BoundSet.empty in

              (* Collect bounds from right bag (c_meta2) - LessEq *) 
              (match v2 with
               | Finite s2 ->
                   FloatSet.iter (fun f -> 
                     bounds_to_add := Bags.BoundSet.add (Bags.Less f) !bounds_to_add
                   ) s2
               | Top -> ()
              );

              (* Collect bounds from left bag (c_meta1) - Less *) 
              (match v1 with
               | Finite s1 ->
                   FloatSet.iter (fun f -> 
                     bounds_to_add := Bags.BoundSet.add (Bags.LessEq f) !bounds_to_add
                   ) s1
               | Top -> ()
              );

              (* Apply collected bounds to b_meta *) 
              if not (Bags.BoundSet.is_empty !bounds_to_add) then
                let current_bound_val = Bags.BoundBag.get b_meta in
                match current_bound_val with
                | Top -> () (* Cannot add to Top *) 
                | Finite current_set ->
                    let new_set = Bags.BoundSet.union current_set !bounds_to_add in
                    if not (Bags.BoundSet.equal current_set new_set) then (
                       (* Update using temporary bag and leq *) 
                       let temp_finite_bag = Bags.BoundBag.create (Finite new_set) in
                       Bags.BoundBag.leq temp_finite_bag b_meta
                    )
        in
        (* Register the combined listener on both float bags *) 
        Bags.FloatBag.listen c_meta1 listener;
        Bags.FloatBag.listen c_meta2 listener;

        (TBool, TAExprNode (Less ((t1,a1), (t2,a2)))) (* Result is TBool *)
      
    | LessEq (e1, e2) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let b_meta = Bags.fresh_bound_bag () in (* Shared bound bag for unification *)
        let c_meta1 = Bags.fresh_float_bag () in
        let c_meta2 = Bags.fresh_float_bag () in
        (try unify t1 (Types.TFloat (b_meta, c_meta1)) (* Unify t1 with TFloat(b_meta, c1) *)
         with Failure msg -> failwith (Printf.sprintf "Type error in Less (<) left operand: %s" msg));
        (try unify t2 (Types.TFloat (b_meta, c_meta2)) (* Unify t2 with TFloat(b_meta, c2) *)
         with Failure msg -> failwith (Printf.sprintf "Type error in Less (<) right operand: %s" msg));

        (* Nested listener logic for LessEq *) 
        let listener () = (* Listener takes unit *)
          let v1 = Bags.FloatBag.get c_meta1 in (* Get value inside listener *)
          let v2 = Bags.FloatBag.get c_meta2 in (* Get value inside listener *)
          match v1, v2 with
          | Top, Top ->
              (* Both Top -> BoundBag should be Top *) 
              Bags.BoundBag.leq (Bags.BoundBag.create Top) b_meta
          | _, _ ->
              (* At least one is not Top. Add bounds from Finite bags. *)
              (* Temporarily store bounds to add *) 
              let bounds_to_add = ref Bags.BoundSet.empty in

              (* Collect bounds from right bag (c_meta2) - LessEq *) 
              (match v2 with
               | Finite s2 ->
                   FloatSet.iter (fun f -> 
                     bounds_to_add := Bags.BoundSet.add (Bags.LessEq f) !bounds_to_add
                   ) s2
               | Top -> ()
              );

              (* Collect bounds from left bag (c_meta1) - Less *) 
              (match v1 with
               | Finite s1 ->
                   FloatSet.iter (fun f -> 
                     bounds_to_add := Bags.BoundSet.add (Bags.Less f) !bounds_to_add
                   ) s1
               | Top -> ()
              );

              (* Apply collected bounds to b_meta *) 
              if not (Bags.BoundSet.is_empty !bounds_to_add) then
                let current_bound_val = Bags.BoundBag.get b_meta in
                match current_bound_val with
                | Top -> () (* Cannot add to Top *) 
                | Finite current_set ->
                    let new_set = Bags.BoundSet.union current_set !bounds_to_add in
                    if not (Bags.BoundSet.equal current_set new_set) then (
                       (* Update using temporary bag and leq *) 
                       let temp_finite_bag = Bags.BoundBag.create (Finite new_set) in
                       Bags.BoundBag.leq temp_finite_bag b_meta
                    )
        in
        (* Register the combined listener on both float bags *) 
        Bags.FloatBag.listen c_meta1 listener;
        Bags.FloatBag.listen c_meta2 listener;

        (TBool, TAExprNode (LessEq ((t1,a1), (t2,a2)))) (* Result is TBool *)

    | If (e1, e2, e3) ->
      let t1, a1 = aux env e1 in
      (try sub_type t1 Types.TBool (* Condition must be bool *) 
       with Failure msg -> failwith ("Type error in If condition: " ^ msg));
      let t2, a2 = aux env e2 in
      let t3, a3 = aux env e3 in
      let result_ty = Types.fresh_meta () in (* Fresh meta for the result *) 
      (try 
         sub_type t2 result_ty; (* Enforce true_branch <: result *) 
         sub_type t3 result_ty  (* Enforce false_branch <: result *) 
       with Failure msg -> failwith ("Type error in If branches: " ^ msg)); 
      (result_ty, TAExprNode (If ((t1,a1), (t2,a2), (t3,a3))))
      
    | Pair (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (TPair (t1, t2), TAExprNode (Pair ((t1, a1), (t2, a2))))
      
    | First e1 ->
      let t, a = aux env e1 in
      let t1_meta = Types.fresh_meta () in
      let t2_meta = Types.fresh_meta () in
      (try sub_type t (TPair (t1_meta, t2_meta))
       with Failure msg -> failwith ("Type error in First (fst): " ^ msg));
      (Types.force t1_meta, TAExprNode (First (t, a))) (* Use Types.force *)
      
    | Second e1 ->
      let t, a = aux env e1 in
      let t1_meta = Types.fresh_meta () in
      let t2_meta = Types.fresh_meta () in
      (try sub_type t (TPair (t1_meta, t2_meta))
       with Failure msg -> failwith ("Type error in Second (snd): " ^ msg));
      (Types.force t2_meta, TAExprNode (Second (t, a))) (* Use Types.force *)
      
    | Fun (x, e1) ->
      let param_type = Types.fresh_meta () in
      let env' = StringMap.add x param_type env in
      let return_type, a = aux env' e1 in
      (Types.TFun (param_type, return_type), TAExprNode (Fun (x, (return_type, a))))
      
    | FuncApp (e1, e2) ->
      let t_fun, a_fun = aux env e1 in
      let t_arg, a_arg = aux env e2 in
      let param_ty_expected = Types.fresh_meta () in (* Fresh meta for expected param type *) 
      let result_ty = Types.fresh_meta () in (* Fresh meta for result type *) 
      (try 
         (* Check t_fun is a function expecting param_ty_expected and returning result_ty *) 
         sub_type t_fun (Types.TFun (param_ty_expected, result_ty));
         (* Check t_arg is a subtype of what the function expects *) 
         sub_type t_arg param_ty_expected 
       with Failure msg -> failwith ("Type error in function application: " ^ msg));
      (result_ty, TAExprNode (FuncApp ((t_fun, a_fun), (t_arg, a_arg))))
      
    | LoopApp (e1, e2, e3) ->
      let t_fun, a_fun = aux env e1 in
      let t_arg, a_arg = aux env e2 in
      (* Third argument is just a number *)
      let param_ty_expected = Types.fresh_meta () in (* Fresh meta for expected param type *) 
      let result_ty = Types.fresh_meta () in (* Fresh meta for result type *) 
      (try 
          (* Check t_fun is a function expecting param_ty_expected and returning result_ty *) 
          sub_type t_fun (Types.TFun (param_ty_expected, result_ty));
          (* Check t_arg is a subtype of what the function expects *) 
          sub_type t_arg param_ty_expected 
        with Failure msg -> failwith ("Type error in loop application: " ^ msg));
      (result_ty, TAExprNode (LoopApp ((t_fun, a_fun), (t_arg, a_arg), e3)))
       
    | FinConst (k, n) ->
      if k < 0 || k >= n then failwith (Printf.sprintf "Invalid FinConst value: %d#%d. k must be >= 0 and < n." k n);
      (Types.TFin n, TAExprNode (FinConst (k, n)))

    | FinLt (e1, e2, n) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinLt modulus: <#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Types.TFin n in
      (try sub_type t1 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinLt (<#%d) left operand: %s" n msg));
      (try sub_type t2 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinLt (<#%d) right operand: %s" n msg));
      (Types.TBool, TAExprNode (FinLt ((t1, a1), (t2, a2), n)))
      
    | FinLeq (e1, e2, n) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinLeq modulus: <=#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Types.TFin n in
      (try sub_type t1 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinLeq (<=#%d) left operand: %s" n msg));
      (try sub_type t2 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinLeq (<=#%d) right operand: %s" n msg));
      (Types.TBool, TAExprNode (FinLeq ((t1, a1), (t2, a2), n)))

    | FinEq (e1, e2, n) -> (* New case for FinEq in elab *)
      if n <= 0 then failwith (Printf.sprintf "Invalid FinEq modulus: ==#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Types.TFin n in
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
      (try sub_type t1 TBool (* Argument must be TBool *)
       with Failure msg -> failwith ("Type error in Observe argument: " ^ msg));
      (TUnit, TAExprNode (Observe (t1, a1))) (* Result is TUnit *)

    | Fix (f, x, e_body) -> 
      let fun_type_itself = Types.fresh_meta () in (* Type of f *)
      let param_type = Types.fresh_meta () in      (* Type of x *)
      let env_body = StringMap.add x param_type (StringMap.add f fun_type_itself env) in
      let body_texpr = aux env_body e_body in
      let body_ret_type = fst body_texpr in
      let actual_fun_type = Types.TFun (param_type, body_ret_type) in
      unify fun_type_itself actual_fun_type;
      (fun_type_itself, TAExprNode (Fix (f, x, body_texpr)))

    | Nil -> 
      let elem_ty = Types.fresh_meta () in
      (TList elem_ty, TAExprNode Nil) 

    | Cons (e_hd, e_tl) ->
      let t_hd, a_hd = aux env e_hd in
      let t_tl, a_tl = aux env e_tl in
      (try unify t_tl (TList t_hd) 
       with Failure msg -> failwith ("Type error in list construction (::): " ^ msg));
      (t_tl, TAExprNode (Cons ((t_hd, a_hd), (t_tl, a_tl))))

    | MatchList (e_match, e_nil, y, ys, e_cons) ->
      let t_match, a_match = aux env e_match in
      let elem_ty = Types.fresh_meta () in
      (try unify t_match (TList elem_ty)
       with Failure msg -> failwith ("Type error in match expression (expected list type): " ^ msg));
      (* Type check nil branch *) 
      let t_nil, a_nil = aux env e_nil in
      (* Type check cons branch *) 
      let env_cons = StringMap.add y elem_ty (StringMap.add ys t_match env) in
      let t_cons, a_cons = aux env_cons e_cons in
      (* Unify branch types *) 
      let result_ty = Types.fresh_meta () in
      (try 
         sub_type t_nil result_ty;
         sub_type t_cons result_ty
       with Failure msg -> failwith ("Type error in match branches: " ^ msg));
      (result_ty, TAExprNode (MatchList ((t_match, a_match), (t_nil, a_nil), y, ys, (t_cons, a_cons))))

    | Ref e1 ->
      let t1, a1 = aux env e1 in
      (TRef t1, TAExprNode (Ref (t1, a1)))

    | Deref e1 ->
      let t1, a1 = aux env e1 in
      let val_ty = Types.fresh_meta () in
      (try unify t1 (TRef val_ty)
       with Failure msg -> failwith ("Type error in dereference (!): " ^ msg));
      (Types.force val_ty, TAExprNode (Deref (t1, a1)))

    | Assign (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let val_ty = Types.fresh_meta () in
      (try 
         unify t1 (TRef val_ty);
         sub_type t2 (Types.force val_ty)
       with Failure msg -> failwith ("Type error in assignment (:=): " ^ msg));
      (TUnit, TAExprNode (Assign ((t1, a1), (t2, a2))))

    | Seq (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (t2, TAExprNode (Seq ((t1, a1), (t2, a2)))) (* Type of sequence is type of e2 *)

    | Unit -> (Types.TUnit, TAExprNode Unit)

    | RuntimeError s -> (Types.fresh_meta (), TAExprNode (RuntimeError s))

  in
  aux StringMap.empty e

(* Function that does elab but insists that the return type is TBool *)
let elab_bool (e : expr) : texpr =
  let t, a = elab e in
  sub_type t Types.TBool;
  (t, a)

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
         | Bags.Finite bound_set_from_context ->
            let idx, modulus = get_const_idx_and_modulus f bound_set_from_context in
            if idx = 0 then (
              if modulus = 1 then (
                let sz = (Util.bit_length (int_of_float f)) in if sz > !Util.curr_max_int_sz then Util.curr_max_int_sz := sz); (* Discrete leaf *)
              ExprNode (FinConst (int_of_float f, modulus))) (* Keep original const if idx is zero *)
            else
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
        | Bags.Top -> (* If outer Sample's bounds are Top, fallback to simple recursive discretization of params *)
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
  discretize e