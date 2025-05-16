(* Main implementation of continuous dice *)

open Types
open Bags (* Open Bags to access FloatSet, BoundSet, FloatBag, BoundBag etc. *)
(* FloatBag is now defined in Bags module *)

(* Re-export internal modules needed by executable/tests *)
module Parse = Parse 
module Pretty = Pretty
module Util = Util 
module Interp = Interp
module Types = Types (* Explicitly alias Types here *)
module Distributions = Distributions (* Explicitly alias Distributions here *)
module Bags = Bags   (* And Bags *)
(* We might need others like Types, Bag, Stats depending on usage, 
   but let's start with these two. *)

module StringMap = Map.Make(String)

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
      (match dist_exp with
      | Uniform (a, b) ->
        (* Recursively translate a and b *)
        let t1, a1 = aux env a in
        let t2, a2 = aux env b in
        let t1_bag1 = Bags.fresh_bound_bag () in
        let t1_bag2 = Bags.fresh_float_bag () in
        let t2_bag1 = Bags.fresh_bound_bag () in
        let t2_bag2 = Bags.fresh_float_bag () in
        (try unify t1 (Types.TFloat (t1_bag1, t1_bag2))
         with Failure msg -> failwith (Printf.sprintf "Type error in Sample (uniform) left operand: %s" msg));
        (try unify t2 (Types.TFloat (t2_bag1, t2_bag2))
         with Failure msg -> failwith (Printf.sprintf "Type error in Sample (uniform) right operand: %s" msg));
        (* Add listener logic for uniform distribution *)
        (* We want to make the output non-discretizable if the input isn't discrete *)
        (* We thus listen to the two input bags and if any of them are non-discrete, we make the output non-discrete *)
        (* We also want to add the floats in the floatbag of the input to the boundbag of the input *)
        (* Let's do that first *)
        let add_floats_to_boundbag (float_bag : FloatBag.bag) (bound_bag : BoundBag.bag) =
          let listener () =
            let v = Bags.FloatBag.get float_bag in
            (match v with
            | Finite s -> Bags.BoundBag.add_all s bound_bag
            | Top -> (* Make it top if it's top *)
              Bags.BoundBag.leq (Bags.BoundBag.create Top) bound_bag)
          in
          Bags.FloatBag.listen float_bag listener in
        add_floats_to_boundbag t1_bag2 bounds_bag_ref;
        add_floats_to_boundbag t2_bag2 bounds_bag_ref;
        (* Now we want to make the output non-discretizable if the input isn't discrete *)
        let make_output_top_if_input_boundbag_is_top input_bound_bag =
          let listener () =
            let v = Bags.BoundBag.get input_bound_bag in
            (match v with
            | Top -> Bags.BoundBag.leq (Bags.BoundBag.create Top) bounds_bag_ref
            | _ -> ())
          in
          Bags.BoundBag.listen input_bound_bag listener in
        make_output_top_if_input_boundbag_is_top t1_bag1;
        make_output_top_if_input_boundbag_is_top t2_bag1;
        
        let dist_exp' = Uniform ((t1, a1), (t2, a2)) in
        (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (Sample dist_exp'))
      | _ -> failwith "Only uniform distributions are supported for elab in CDice")
      
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
      
    | App (e1, e2) ->
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
      (result_ty, TAExprNode (App ((t_fun, a_fun), (t_arg, a_arg))))
      
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
        let bounds_bag = (match Types.force ty with
          | Types.TFloat (b, _) -> b (* Extract bounds bag *)
          | _ -> failwith "Type error: Const expects float") in
        let set_or_top_val = Bags.BoundBag.get bounds_bag in
        (match set_or_top_val with
         | Bags.Top -> ExprNode (Const f) (* Keep original if Top *)
         | Bags.Finite bound_set ->
              let cuts = Bags.BoundSet.elements bound_set in
              let bound_matches_float b x = match b with
                | Bags.Less c -> x < c
                | Bags.LessEq c -> x <= c
              in
              (* Find the index of the float in the bag *)
              let idx = List.length (List.filter (fun x -> not (bound_matches_float x f)) cuts) in
              (* Generate FinConst expression *)
              ExprNode (FinConst (idx, 1+List.length cuts)))

    | BoolConst b -> ExprNode (BoolConst b)

    | Var x ->
        ExprNode (Var x)

    | Let (x, te1, te2) ->
        ExprNode (Let (x, aux te1, aux te2))

    | Sample dist_exp ->

          let bounds_bag =
            match Types.force ty with 
            | Types.TFloat (b, _) -> b (* Extract bounds bag *)
            | _ -> failwith "Internal error: Sample not TFloat"
          in
          let set_or_top_val = Bags.BoundBag.get bounds_bag in 
          (match set_or_top_val with
          | Bags.Top -> (* Keep original if Top *) 
            (match dist_exp with 
            | Uniform (texpr_a, texpr_b) ->
                let a = aux texpr_a in
                let b = aux texpr_b in
                ExprNode (Sample (Uniform (a, b)))
            | Gaussian (texpr_mu, texpr_sigma) ->
                let mu = aux texpr_mu in
                let sigma = aux texpr_sigma in
                ExprNode (Sample (Gaussian (mu, sigma)))
            | Exponential (texpr_lambda) ->
                let lambda = aux texpr_lambda in
                ExprNode (Sample (Exponential (lambda)))
            | _ -> failwith "Internal error: Sample not Uniform, Gaussian, or Exponential")
          | Bags.Finite bound_set -> 
            (* 
              case_on_dist_exp is a function that takes a dist_exp (distribution expression),
              and checks if its parameters have a finite number of possibilities (bounds bag isn't Top).
              If bounds_bag is Top, returns the default case (Sample dist_exp).
              If bounds_bag is Finite, discretizes the distribution using the cuts, 
              looking up the exact value in each interval in the floats bag,
              and calls the function with that distribution.
              Below, we then discretize that distribution.
            *)
            case_on_dist_exp dist_exp ~default:(Sample dist_exp) ~cases:(fun distr -> 
              (* Discretize using cuts *) 
              let cuts = 
                Bags.BoundSet.elements bound_set 
                |> List.map (function Bags.Less c -> c | Bags.LessEq c -> c) 
                |> List.sort compare
              in 
              let intervals = List.init (List.length cuts + 1) (fun i ->
                let left = if i = 0 then neg_infinity else List.nth cuts (i - 1) in
                let right = if i = List.length cuts then infinity else List.nth cuts i in
                (left, right)
              ) in
              let probs = List.map (fun (left, right) ->
                prob_cdistr_interval left right dist
              ) intervals in
              (* Convert to DistrCase with FinConst expressions *) 
              let n = List.length probs in (* Number of intervals = modulus *)
              if n = 0 then failwith "Internal error: discretization resulted in zero intervals";
              let cases = List.mapi (fun i prob -> (ExprNode (FinConst (i, n)), prob)) probs in
              ExprNode (DistrCase cases)))
        
    | DistrCase cases ->
      (* Recursively discretize the expressions within the cases *) 
      let discretized_cases = 
        List.map (fun (texpr, prob) -> (aux texpr, prob)) cases 
      in
      ExprNode (DistrCase discretized_cases)

    | Less (te1, te2) -> (* Match the typed expressions te1, te2 *) 
        let t1 = fst te1 in (* Extract type t1 *) 
        let t2 = fst te2 in (* Extract type t2 *)
        let b1 = (match Types.force t1 with (* Get bound bag b1 from t1 *)
          | Types.TFloat (b, _) -> b 
          | _ -> failwith "Type error: Less expects float on left operand") 
        in
        let b2 = (match Types.force t2 with (* Get bound bag b2 from t2 *)
          | Types.TFloat (b, _) -> b
          | _ -> failwith "Type error: Less expects float on right operand")
        in
        (* Get bag values and compare using BoundSetContents.equal *) 
        let val1 = Bags.BoundBag.get b1 in
        let val2 = Bags.BoundBag.get b2 in
        if not (Bags.BoundSetContents.equal val1 val2) then 
          failwith "Internal error: Less operands have different bound bag values despite elaboration";

        (* Use val1 (since val1 = val2) *) 
        (match val1 with 
          | Bags.Top -> 
              (* If bounds are Top, don't discretize, keep original Less structure *) 
              ExprNode (Less (aux te1, aux te2)) 
          | Bags.Finite bound_set -> 
              (* Discretize based on shared bounds *) 
              let cuts = Bags.BoundSet.elements bound_set in
              let n = List.length cuts + 1 in (* Modulus *) 
              if n <= 0 then failwith "Internal error: discretization resulted in zero or negative intervals for Less";
              let d1 = aux te1 in (* Discretize operands *)
              let d2 = aux te2 in 
              ExprNode (FinLt (d1, d2, n))) (* Generate FinLt *) 
        
    | LessEq (te1, te2) -> (* Match the typed expressions te1, te2 *) 
        let t1 = fst te1 in (* Extract type t1 *) 
        let t2 = fst te2 in (* Extract type t2 *) 
        let b1 = (match Types.force t1 with (* Get bound bag b1 from t1 *)
          | Types.TFloat (b, _) -> b 
          | _ -> failwith "Type error: LessEq expects float on left operand") 
        in
        let b2 = (match Types.force t2 with (* Get bound bag b2 from t2 *)
          | Types.TFloat (b, _) -> b
          | _ -> failwith "Type error: LessEq expects float on right operand")
        in
        (* Get bag values and compare using BoundSetContents.equal *) 
        let val1 = Bags.BoundBag.get b1 in
        let val2 = Bags.BoundBag.get b2 in
        if not (Bags.BoundSetContents.equal val1 val2) then 
          failwith "Internal error: LessEq operands have different bound bag values despite elaboration";
        
        (* Use val1 (since val1 = val2) *) 
        (match val1 with 
          | Bags.Top -> 
              (* If bounds are Top, don't discretize, keep original LessEq structure *) 
              ExprNode (LessEq (aux te1, aux te2))
          | Bags.Finite bound_set -> 
              (* Discretize based on shared bounds *) 
              let cuts = Bags.BoundSet.elements bound_set in
              let n = List.length cuts + 1 in (* Modulus *) 
              if n <= 0 then failwith "Internal error: discretization resulted in zero or negative intervals for LessEq";
              let d1 = aux te1 in (* Discretize operands *)
              let d2 = aux te2 in 
              ExprNode (FinLeq (d1, d2, n))) (* Generate FinLeq *) 

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
        
    | App (te1, te2) ->
        ExprNode (App (aux te1, aux te2))

    (* Fin types are already discrete, pass them through *) 
    | FinConst (k, n) -> 
        ExprNode (FinConst (k, n))
    | FinLt (te1, te2, n) -> 
        ExprNode (FinLt (aux te1, aux te2, n))
    | FinLeq (te1, te2, n) -> 
        ExprNode (FinLeq (aux te1, aux te2, n))

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

  in
  aux e