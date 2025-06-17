(* Type inference and elaboration for continuous dice *)

open Ast
open Lats

module StringMap = Map.Make(String)

(* Enforce t_sub is a subtype of t_super *)
let rec sub_type (t_sub : ty) (t_super : ty) : unit =
  match Ast.force t_sub, Ast.force t_super with
  (* Base Cases *)
  | TBool,    TBool      -> ()
  | TFin n1, TFin n2 when n1 = n2 -> () 
  | TUnit, TUnit -> ()
  (* Structural Cases *)
  | TFloat (b1, c1), TFloat (b2, c2) -> 
      Lats.CutLat.eq b1 b2;  (* Bounds must be consistent *) 
      Lats.FloatLat.leq c1 c2  (* Constants flow sub -> super *) 
  | TPair(a1, b1), TPair(a2, b2) -> 
      sub_type a1 a2; (* Covariant *) 
      sub_type b1 b2  (* Covariant *) 
  | TFun(a1, b1), TFun(a2, b2) -> 
      sub_type a2 a1; (* Contravariant argument *) 
      sub_type b1 b2  (* Covariant result *) 
  | TList t1, TList t2 -> sub_type t1 t2 (* Covariant *) 
  | TRef t1, TRef t2 -> unify t1 t2 (* Invariant *)
  | TMeta r, _ ->
    (match Ast.force t_super with (* Ensure t_super is forced *)
    | TMeta r' -> (Ast.listen r (fun t -> sub_type t t_super); Ast.listen r' (fun t' -> sub_type t_sub t'))
    | TBool -> Ast.assign r TBool
    | TFin n -> Ast.assign r (TFin n)
    | TPair (_, _) -> let a_meta = Ast.fresh_meta () in let b_meta = Ast.fresh_meta () in
      Ast.assign r (TPair (a_meta, b_meta)); sub_type t_sub t_super
    | TFun (_, _) -> let a_meta = Ast.fresh_meta () in let b_meta = Ast.fresh_meta () in
      Ast.assign r (TFun (a_meta, b_meta)); sub_type t_sub t_super
    | TFloat (_, _) -> let b_bag = Lats.fresh_cut_bag () in let c_bag = Lats.fresh_float_bag () in
      Ast.assign r (TFloat (b_bag, c_bag)); sub_type t_sub t_super
    | TUnit -> Ast.assign r TUnit (* Handle TUnit for t_super *)
    | TList _ -> let elem_meta = Ast.fresh_meta () in 
                 Ast.assign r (TList elem_meta); sub_type t_sub t_super
    | TRef _ -> let ref_meta = Ast.fresh_meta () in
                Ast.assign r (TRef ref_meta); sub_type t_sub t_super
    )
  | _, TMeta r ->
    (match Ast.force t_sub with (* Ensure t_sub is forced *)
    | TMeta r' -> (Ast.listen r (fun t -> sub_type t_sub t); Ast.listen r' (fun t' -> sub_type t_sub t'))
    | TBool -> Ast.assign r TBool
    | TFin n -> Ast.assign r (TFin n)
    | TPair (_, _) -> let a_meta = Ast.fresh_meta () in let b_meta = Ast.fresh_meta () in
      Ast.assign r (TPair (a_meta, b_meta)); sub_type t_sub t_super
    | TFun (_, _) -> let a_meta = Ast.fresh_meta () in let b_meta = Ast.fresh_meta () in
      Ast.assign r (TFun (a_meta, b_meta)); sub_type t_sub t_super
    | TFloat (_, _) -> let b_bag = Lats.fresh_cut_bag () in let c_bag = Lats.fresh_float_bag () in
      Ast.assign r (TFloat (b_bag, c_bag)); sub_type t_sub t_super
    | TUnit -> Ast.assign r TUnit (* Handle TUnit for t_sub *)
    | TList _ -> let elem_meta = Ast.fresh_meta () in 
                 Ast.assign r (TList elem_meta); sub_type t_sub t_super
    | TRef _ -> let ref_meta = Ast.fresh_meta () in
                Ast.assign r (TRef ref_meta); sub_type t_sub t_super
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

(* Type inference and elaboration: expr -> texpr, generating bag constraints and performing type checking *)
let infer (e : expr) : texpr =
  let rec aux (env : ty StringMap.t) (ExprNode e_node : expr) : texpr =
    match e_node with
    | Const f -> 
      (* Constant float: Create bag refs *) 
      let cuts_bag_ref = Lats.CutLat.create (Finite CutSet.empty) in 
      let consts_bag_ref = Lats.FloatLat.create (Finite (FloatSet.singleton f)) in
      (TFloat (cuts_bag_ref, consts_bag_ref), TAExprNode (Const f))
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
      let cuts_bag_ref = Lats.CutLat.create (Finite CutSet.empty) in 
      let consts_bag_ref = Lats.FloatLat.create Top in 

      (* Helper to propagate float constants from an argument to its own bound bag *)
      let add_floats_to_cutbag (float_bag : FloatLat.bag) (cut_bag : CutLat.bag) =
        let listener () =
          let v = Lats.FloatLat.get float_bag in
          (match v with
          | Finite s -> Lats.CutLat.add_all s cut_bag
            | Top -> Lats.CutLat.leq (Lats.CutLat.create Top) cut_bag)
        in
        Lats.FloatLat.listen float_bag listener
      in

      (* Helper to make the output distribution's bound bag Top if any input's bound bag becomes Top *)
      let make_output_top_if_input_boundbag_is_top input_cut_bag =
        let listener () =
          let v = Lats.CutLat.get input_cut_bag in
          (match v with
          | Top -> Lats.CutLat.leq (Lats.CutLat.create Top) cuts_bag_ref (* cuts_bag_ref is from Sample scope *)
          | _ -> ())
        in
        Lats.CutLat.listen input_cut_bag listener
      in

      (* Helper to make an input's bound bag Top if the output distribution's bound bag becomes Top *)
      let make_input_top_if_output_boundbag_is_top input_cut_bag output_cut_bag =
        let listener () =
          let v = Lats.CutLat.get output_cut_bag in
          (match v with
          | Top -> Lats.CutLat.leq (Lats.CutLat.create Top) input_cut_bag
          | _ -> ())
        in
        Lats.CutLat.listen output_cut_bag listener
      in

      (match dist_exp with
      | Distr1 (dist_kind, arg_e) ->
          let t_arg, a_arg = aux env arg_e in
          let t_arg_cut_bag = Lats.fresh_cut_bag () in
          let t_arg_float_bag = Lats.fresh_float_bag () in
          (try unify t_arg (Ast.TFloat (t_arg_cut_bag, t_arg_float_bag))
           with Failure msg -> 
            let kind_str = Pretty.string_of_expr_indented (ExprNode (Sample (Distr1 (dist_kind, arg_e)))) in 
            failwith (Printf.sprintf "Type error in Sample (%s) argument: %s" kind_str msg));
          
          add_floats_to_cutbag t_arg_float_bag t_arg_cut_bag;
          make_output_top_if_input_boundbag_is_top t_arg_cut_bag;
          make_input_top_if_output_boundbag_is_top t_arg_cut_bag cuts_bag_ref;

          let dist_exp' = Distr1 (dist_kind, (t_arg, a_arg)) in
          (TFloat (cuts_bag_ref, consts_bag_ref), TAExprNode (Sample dist_exp'))

      | Distr2 (dist_kind, arg1_e, arg2_e) ->
        let t1, a1 = aux env arg1_e in
        let t2, a2 = aux env arg2_e in
        let t1_cut_bag = Lats.fresh_cut_bag () in
        let t1_float_bag = Lats.fresh_float_bag () in
        let t2_cut_bag = Lats.fresh_cut_bag () in
        let t2_float_bag = Lats.fresh_float_bag () in

        (try unify t1 (Ast.TFloat (t1_cut_bag, t1_float_bag))
         with Failure msg -> 
          let kind_str = Pretty.string_of_expr_indented (ExprNode (Sample (Distr2 (dist_kind, arg1_e, arg2_e)))) in
          failwith (Printf.sprintf "Type error in Sample (%s) first argument: %s" kind_str msg));
        (try unify t2 (Ast.TFloat (t2_cut_bag, t2_float_bag))
         with Failure msg -> 
          let kind_str = Pretty.string_of_expr_indented (ExprNode (Sample (Distr2 (dist_kind, arg1_e, arg2_e)))) in
          failwith (Printf.sprintf "Type error in Sample (%s) second argument: %s" kind_str msg));
        
        add_floats_to_cutbag t1_float_bag t1_cut_bag;
        add_floats_to_cutbag t2_float_bag t2_cut_bag;

        make_output_top_if_input_boundbag_is_top t1_cut_bag;
        make_output_top_if_input_boundbag_is_top t2_cut_bag;

        make_input_top_if_output_boundbag_is_top t1_cut_bag cuts_bag_ref;
        make_input_top_if_output_boundbag_is_top t2_cut_bag cuts_bag_ref;

        let dist_exp' = Distr2 (dist_kind, (t1, a1), (t2, a2)) in
        (TFloat (cuts_bag_ref, consts_bag_ref), TAExprNode (Sample dist_exp'))
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
      let result_ty = Ast.fresh_meta () in (* Fresh meta for the result *) 
      List.iter (fun ((branch_ty, _), _) -> 
        try sub_type branch_ty result_ty (* Enforce branch <: result *)
        with Failure msg -> failwith ("Type error in DistrCase branches: " ^ msg)
      ) typed_cases;
      
      let annotated_cases = List.map (fun (texpr, prob) -> (texpr, prob)) typed_cases in
      (result_ty, TAExprNode (DistrCase annotated_cases))

    | Cmp (cmp_op, e1, e2, flipped) ->
        let t1, a1 = aux env e1 in
        let t2, a2 = aux env e2 in
        let b_meta = Lats.fresh_cut_bag () in (* Shared bound bag for unification *)
        let c_meta1 = Lats.fresh_float_bag () in
        let c_meta2 = Lats.fresh_float_bag () in
        (try unify t1 (Ast.TFloat (b_meta, c_meta1)) (* Unify t1 with TFloat(b_meta, c1) *)
         with Failure msg -> failwith (Printf.sprintf "Type error in comparison left operand: %s" msg));
        (try unify t2 (Ast.TFloat (b_meta, c_meta2)) (* Unify t2 with TFloat(b_meta, c2) *)
         with Failure msg -> failwith (Printf.sprintf "Type error in comparison right operand: %s" msg));

        (* Nested listener logic for comparison *) 
        let listener () = (* Listener takes unit *)
          let v1 = Lats.FloatLat.get c_meta1 in (* Get value inside listener *)
          let v2 = Lats.FloatLat.get c_meta2 in (* Get value inside listener *)
          match v1, v2 with
          | Top, Top ->
              (* Both Top -> BoundBag should be Top *) 
              Lats.CutLat.leq (Lats.CutLat.create Top) b_meta
          | Finite _, Finite s2 ->
              (* Both are not Top. This means that e2 is being compared to a discrete distribution. *)
              (* Only collect the cuts from the right bag, the constant itself *)
              (* Temporarily store cuts to add *)
              let cuts_to_add = ref Lats.CutSet.empty in

              FloatSet.iter (fun f -> 
                let cut = match cmp_op with
                  | Ast.Lt -> Lats.Less f
                  | Ast.Le -> Lats.LessEq f
                in
                cuts_to_add := Lats.CutSet.add cut !cuts_to_add
              ) s2;

              (* Apply collected cuts to b_meta *) 
              (match Lats.CutLat.get b_meta with
              | Top -> ()  (* Cannot add to Top *) 
              | Finite current_set ->
                  let new_set = Lats.CutSet.union current_set !cuts_to_add in
                  if not (Lats.CutSet.equal current_set new_set) then
                    let temp_finite_bag = Lats.CutLat.create (Finite new_set) in
                    Lats.CutLat.leq temp_finite_bag b_meta
              )
          | _, _ ->
              (* Only one is Top. This means that e2 is being compared to a continuous distribution. *)
              (* Add cuts from Finite bags. *)
              (* Temporarily store cuts to add *) 
              let cuts_to_add = ref Lats.CutSet.empty in

              (* Collect cuts from right bag (c_meta2) *) 
              (match v2 with
               | Finite s2 ->
                   FloatSet.iter (fun f -> 
                     let cut = match cmp_op with
                       | Ast.Lt -> Lats.Less f
                       | Ast.Le -> Lats.LessEq f
                     in
                     cuts_to_add := Lats.CutSet.add cut !cuts_to_add
                   ) s2
               | Top -> ()
              );

              (* Collect cuts from left bag (c_meta1) *) 
              (match v1 with
               | Finite s1 ->
                   FloatSet.iter (fun f -> 
                     let cut = match cmp_op with
                       | Ast.Lt -> Lats.LessEq f
                       | Ast.Le -> Lats.Less f
                     in
                     cuts_to_add := Lats.CutSet.add cut !cuts_to_add
                   ) s1
               | Top -> ()
              );

              (* Apply collected cuts to b_meta *) 
              if not (Lats.CutSet.is_empty !cuts_to_add) then
                let current_cut_val = Lats.CutLat.get b_meta in
                match current_cut_val with
                | Top -> () (* Cannot add to Top *) 
                | Finite current_set ->
                    let new_set = Lats.CutSet.union current_set !cuts_to_add in
                    if not (Lats.CutSet.equal current_set new_set) then (
                       (* Update using temporary bag and leq *) 
                       let temp_finite_bag = Lats.CutLat.create (Finite new_set) in
                       Lats.CutLat.leq temp_finite_bag b_meta
                    )
        in
        (* Register the combined listener on both float bags *) 
        Lats.FloatLat.listen c_meta1 listener;
        Lats.FloatLat.listen c_meta2 listener;

        (TBool, TAExprNode (Cmp (cmp_op, (t1,a1), (t2,a2), flipped))) (* Result is TBool, preserve flip flag *)

    | FinCmp (cmp_op, e1, e2, n, flipped) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinCmp modulus: ==#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Ast.TFin n in
      (try sub_type t1 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinCmp (==#%d) left operand: %s" n msg));
      (try sub_type t2 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinCmp (==#%d) right operand: %s" n msg));
      (Ast.TBool, TAExprNode (FinCmp (cmp_op, (t1, a1), (t2, a2), n, flipped)))

    | If (e1, e2, e3) ->
      let t1, a1 = aux env e1 in
      (try sub_type t1 Ast.TBool (* Condition must be bool *) 
       with Failure msg -> failwith ("Type error in If condition: " ^ msg));
      let t2, a2 = aux env e2 in
      let t3, a3 = aux env e3 in
      let result_ty = Ast.fresh_meta () in (* Fresh meta for the result *) 
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
      let t1_meta = Ast.fresh_meta () in
      let t2_meta = Ast.fresh_meta () in
      (try sub_type t (TPair (t1_meta, t2_meta))
       with Failure msg -> failwith ("Type error in First (fst): " ^ msg));
      (Ast.force t1_meta, TAExprNode (First (t, a))) (* Use Ast.force *)
      
    | Second e1 ->
      let t, a = aux env e1 in
      let t1_meta = Ast.fresh_meta () in
      let t2_meta = Ast.fresh_meta () in
      (try sub_type t (TPair (t1_meta, t2_meta))
       with Failure msg -> failwith ("Type error in Second (snd): " ^ msg));
      (Ast.force t2_meta, TAExprNode (Second (t, a))) (* Use Ast.force *)
      
    | Fun (x, e1) ->
      let param_type = Ast.fresh_meta () in
      let env' = StringMap.add x param_type env in
      let return_type, a = aux env' e1 in
      (Ast.TFun (param_type, return_type), TAExprNode (Fun (x, (return_type, a))))
      
    | FuncApp (e1, e2) ->
      let t_fun, a_fun = aux env e1 in
      let t_arg, a_arg = aux env e2 in
      let param_ty_expected = Ast.fresh_meta () in (* Fresh meta for expected param type *) 
      let result_ty = Ast.fresh_meta () in (* Fresh meta for result type *) 
      (try 
         (* Check t_fun is a function expecting param_ty_expected and returning result_ty *) 
         sub_type t_fun (Ast.TFun (param_ty_expected, result_ty));
         (* Check t_arg is a subtype of what the function expects *) 
         sub_type t_arg param_ty_expected 
       with Failure msg -> failwith ("Type error in function application: " ^ msg));
      (result_ty, TAExprNode (FuncApp ((t_fun, a_fun), (t_arg, a_arg))))
      
    | LoopApp (e1, e2, e3) ->
      let t_fun, a_fun = aux env e1 in
      let t_arg, a_arg = aux env e2 in
      (* Third argument is just a number *)
      let param_ty_expected = Ast.fresh_meta () in (* Fresh meta for expected param type *) 
      let result_ty = Ast.fresh_meta () in (* Fresh meta for result type *) 
      (try 
          (* Check t_fun is a function expecting param_ty_expected and returning result_ty *) 
          sub_type t_fun (Ast.TFun (param_ty_expected, result_ty));
          (* Check t_arg is a subtype of what the function expects *) 
          sub_type t_arg param_ty_expected 
        with Failure msg -> failwith ("Type error in loop application: " ^ msg));
      (result_ty, TAExprNode (LoopApp ((t_fun, a_fun), (t_arg, a_arg), e3)))
       
    | FinConst (k, n) ->
      if k < 0 || k >= n then failwith (Printf.sprintf "Invalid FinConst value: %d#%d. k must be >= 0 and < n." k n);
      (Ast.TFin n, TAExprNode (FinConst (k, n)))

    | FinEq (e1, e2, n) -> (* New case for FinEq in elab *)
      if n <= 0 then failwith (Printf.sprintf "Invalid FinEq modulus: ==#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Ast.TFin n in
      (try sub_type t1 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinEq (==#%d) left operand: %s" n msg));
      (try sub_type t2 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinEq (==#%d) right operand: %s" n msg));
      (Ast.TBool, TAExprNode (FinEq ((t1, a1), (t2, a2), n)))

    | And (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (try sub_type t1 Ast.TBool
       with Failure msg -> failwith ("Type error in And (&&) left operand: " ^ msg));
      (try sub_type t2 Ast.TBool
       with Failure msg -> failwith ("Type error in And (&&) right operand: " ^ msg));
      (TBool, TAExprNode (And ((t1, a1), (t2, a2))))
      
    | Or (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (try sub_type t1 Ast.TBool
       with Failure msg -> failwith ("Type error in Or (||) left operand: " ^ msg));
      (try sub_type t2 Ast.TBool
       with Failure msg -> failwith ("Type error in Or (||) right operand: " ^ msg));
      (TBool, TAExprNode (Or ((t1, a1), (t2, a2))))

    | Not e1 ->
      let t1, a1 = aux env e1 in
      (try sub_type t1 Ast.TBool
       with Failure msg -> failwith ("Type error in Not operand: " ^ msg));
      (TBool, TAExprNode (Not (t1, a1)))

    | Observe e1 ->
      let t1, a1 = aux env e1 in
      (try sub_type t1 TBool (* Argument must be TBool *)
       with Failure msg -> failwith ("Type error in Observe argument: " ^ msg));
      (TUnit, TAExprNode (Observe (t1, a1))) (* Result is TUnit *)

    | Fix (f, x, e_body) -> 
      let fun_type_itself = Ast.fresh_meta () in (* Type of f *)
      let param_type = Ast.fresh_meta () in      (* Type of x *)
      let env_body = StringMap.add x param_type (StringMap.add f fun_type_itself env) in
      let body_texpr = aux env_body e_body in
      let body_ret_type = fst body_texpr in
      let actual_fun_type = Ast.TFun (param_type, body_ret_type) in
      unify fun_type_itself actual_fun_type;
      (fun_type_itself, TAExprNode (Fix (f, x, body_texpr)))

    | Nil -> 
      let elem_ty = Ast.fresh_meta () in
      (TList elem_ty, TAExprNode Nil) 

    | Cons (e_hd, e_tl) ->
      let t_hd, a_hd = aux env e_hd in
      let t_tl, a_tl = aux env e_tl in
      (try unify t_tl (TList t_hd) 
       with Failure msg -> failwith ("Type error in list construction (::): " ^ msg));
      (t_tl, TAExprNode (Cons ((t_hd, a_hd), (t_tl, a_tl))))

    | MatchList (e_match, e_nil, y, ys, e_cons) ->
      let t_match, a_match = aux env e_match in
      let elem_ty = Ast.fresh_meta () in
      (try unify t_match (TList elem_ty)
       with Failure msg -> failwith ("Type error in match expression (expected list type): " ^ msg));
      (* Type check nil branch *) 
      let t_nil, a_nil = aux env e_nil in
      (* Type check cons branch *) 
      let env_cons = StringMap.add y elem_ty (StringMap.add ys t_match env) in
      let t_cons, a_cons = aux env_cons e_cons in
      (* Unify branch types *) 
      let result_ty = Ast.fresh_meta () in
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
      let val_ty = Ast.fresh_meta () in
      (try unify t1 (TRef val_ty)
       with Failure msg -> failwith ("Type error in dereference (!): " ^ msg));
      (Ast.force val_ty, TAExprNode (Deref (t1, a1)))

    | Assign (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let val_ty = Ast.fresh_meta () in
      (try 
         unify t1 (TRef val_ty);
         sub_type t2 (Ast.force val_ty)
       with Failure msg -> failwith ("Type error in assignment (:=): " ^ msg));
      (TUnit, TAExprNode (Assign ((t1, a1), (t2, a2))))

    | Seq (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (t2, TAExprNode (Seq ((t1, a1), (t2, a2)))) (* Type of sequence is type of e2 *)

    | Unit -> (Ast.TUnit, TAExprNode Unit)

    | RuntimeError s -> (Ast.fresh_meta (), TAExprNode (RuntimeError s))

  in
  aux StringMap.empty e

(* Function that does infer but insists that the return type is TBool *)
let infer_bool (e : expr) : texpr =
  let t, a = infer e in
  sub_type t Ast.TBool;
  (t, a) 