(* Type inference and elaboration for continuous dice *)

open Types
open Bags

module StringMap = Map.Make(String)

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

(* Type inference and elaboration: expr -> texpr, generating bag constraints and performing type checking *)
let infer (e : expr) : texpr =
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
          | Finite _, Finite s2 ->
              (* Both are not Top. This means that e2 is being compared to a discrete distribution. *)
              (* Only collect the bounds from the right bag, the constant itself *)
              (* Temporarily store bounds to add *)
              let bounds_to_add = ref Bags.BoundSet.empty in

              FloatSet.iter (fun f -> 
                bounds_to_add := Bags.BoundSet.add (Bags.Less f) !bounds_to_add
              ) s2;

              (* Apply collected bounds to b_meta *) 
              (match Bags.BoundBag.get b_meta with
              | Top -> ()  (* Cannot add to Top *) 
              | Finite current_set ->
                  let new_set = Bags.BoundSet.union current_set !bounds_to_add in
                  if not (Bags.BoundSet.equal current_set new_set) then
                    let temp_finite_bag = Bags.BoundBag.create (Finite new_set) in
                    Bags.BoundBag.leq temp_finite_bag b_meta
              )
          | _, _ ->
              (* Only one is Top. This means that e2 is being compared to a continuous distribution. *)
              (* Add bounds from Finite bags. *)
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
          | Finite _, Finite s2 ->
              (* Both are not Top. This means that e2 is being compared to a discrete distribution. *)
              (* Only collect the bounds from the right bag, the constant itself *)
              (* Temporarily store bounds to add *)
              let bounds_to_add = ref Bags.BoundSet.empty in

              FloatSet.iter (fun f -> 
                bounds_to_add := Bags.BoundSet.add (Bags.LessEq f) !bounds_to_add
              ) s2;

              (* Apply collected bounds to b_meta *) 
              (match Bags.BoundBag.get b_meta with
              | Top -> ()  (* Cannot add to Top *) 
              | Finite current_set ->
                  let new_set = Bags.BoundSet.union current_set !bounds_to_add in
                  if not (Bags.BoundSet.equal current_set new_set) then
                    let temp_finite_bag = Bags.BoundBag.create (Finite new_set) in
                    Bags.BoundBag.leq temp_finite_bag b_meta
              )
          | _, _ ->
              (* Only one is Top. This means that e2 is being compared to a continuous distribution. *)
              (* Add bounds from Finite bags. *)
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

    | Greater (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let b_meta = Bags.fresh_bound_bag () in (* Shared bound bag for unification *)
      let c_meta1 = Bags.fresh_float_bag () in
      let c_meta2 = Bags.fresh_float_bag () in
      (try unify t1 (Types.TFloat (b_meta, c_meta1)) (* Unify t1 with TFloat(b_meta, c1) *)
        with Failure msg -> failwith (Printf.sprintf "Type error in Greater (>) left operand: %s" msg));
      (try unify t2 (Types.TFloat (b_meta, c_meta2)) (* Unify t2 with TFloat(b_meta, c2) *)
        with Failure msg -> failwith (Printf.sprintf "Type error in Greater (>) right operand: %s" msg));

      (* Nested listener logic for Greater *) 
      let listener () = (* Listener takes unit *)
        let v1 = Bags.FloatBag.get c_meta1 in (* Get value inside listener *)
        let v2 = Bags.FloatBag.get c_meta2 in (* Get value inside listener *)
        match v1, v2 with
        | Top, Top ->
            (* Both Top -> BoundBag should be Top *) 
            Bags.BoundBag.leq (Bags.BoundBag.create Top) b_meta
        | Finite _, Finite s2 ->
            (* Both are not Top. This means that e2 is being compared to a discrete distribution. *)
            (* Only collect the bounds from the right bag, the constant itself *)
            (* Temporarily store bounds to add *)
            let bounds_to_add = ref Bags.BoundSet.empty in

            FloatSet.iter (fun f -> 
              bounds_to_add := Bags.BoundSet.add (Bags.Greater f) !bounds_to_add
            ) s2;

            (* Apply collected bounds to b_meta *) 
            (match Bags.BoundBag.get b_meta with
            | Top -> ()  (* Cannot add to Top *) 
            | Finite current_set ->
                let new_set = Bags.BoundSet.union current_set !bounds_to_add in
                if not (Bags.BoundSet.equal current_set new_set) then
                  let temp_finite_bag = Bags.BoundBag.create (Finite new_set) in
                  Bags.BoundBag.leq temp_finite_bag b_meta
            )
        | _, _ ->
            (* Only one is Top. This means that e2 is being compared to a continuous distribution. *)
            (* Add bounds from Finite bags. *)
            (* Temporarily store bounds to add *) 
            let bounds_to_add = ref Bags.BoundSet.empty in

            (* Collect bounds from right bag (c_meta2) - GreaterEq *) 
            (match v2 with
              | Finite s2 ->
                  FloatSet.iter (fun f -> 
                    bounds_to_add := Bags.BoundSet.add (Bags.Greater f) !bounds_to_add
                  ) s2
              | Top -> ()
            );

            (* Collect bounds from left bag (c_meta1) - Greater *) 
            (match v1 with
              | Finite s1 ->
                  FloatSet.iter (fun f -> 
                    bounds_to_add := Bags.BoundSet.add (Bags.GreaterEq f) !bounds_to_add
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

      (TBool, TAExprNode (Greater ((t1,a1), (t2,a2)))) (* Result is TBool *)

    | GreaterEq (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let b_meta = Bags.fresh_bound_bag () in (* Shared bound bag for unification *)
      let c_meta1 = Bags.fresh_float_bag () in
      let c_meta2 = Bags.fresh_float_bag () in
      (try unify t1 (Types.TFloat (b_meta, c_meta1)) (* Unify t1 with TFloat(b_meta, c1) *)
        with Failure msg -> failwith (Printf.sprintf "Type error in Greater (>) left operand: %s" msg));
      (try unify t2 (Types.TFloat (b_meta, c_meta2)) (* Unify t2 with TFloat(b_meta, c2) *)
        with Failure msg -> failwith (Printf.sprintf "Type error in Greater (>) right operand: %s" msg));

      (* Nested listener logic for LessEq *) 
      let listener () = (* Listener takes unit *)
        let v1 = Bags.FloatBag.get c_meta1 in (* Get value inside listener *)
        let v2 = Bags.FloatBag.get c_meta2 in (* Get value inside listener *)
        match v1, v2 with
        | Top, Top ->
            (* Both Top -> BoundBag should be Top *) 
            Bags.BoundBag.leq (Bags.BoundBag.create Top) b_meta
        | Finite _, Finite s2 ->
            (* Both are not Top. This means that e2 is being compared to a discrete distribution. *)
            (* Only collect the bounds from the right bag, the constant itself *)
            (* Temporarily store bounds to add *)
            let bounds_to_add = ref Bags.BoundSet.empty in

            FloatSet.iter (fun f -> 
              bounds_to_add := Bags.BoundSet.add (Bags.GreaterEq f) !bounds_to_add
            ) s2;

            (* Apply collected bounds to b_meta *) 
            (match Bags.BoundBag.get b_meta with
            | Top -> ()  (* Cannot add to Top *) 
            | Finite current_set ->
                let new_set = Bags.BoundSet.union current_set !bounds_to_add in
                if not (Bags.BoundSet.equal current_set new_set) then
                  let temp_finite_bag = Bags.BoundBag.create (Finite new_set) in
                  Bags.BoundBag.leq temp_finite_bag b_meta
            )
        | _, _ ->
            (* Only one is Top. This means that e2 is being compared to a continuous distribution. *)
            (* Add bounds from Finite bags. *)
            (* Temporarily store bounds to add *) 
            let bounds_to_add = ref Bags.BoundSet.empty in

            (* Collect bounds from right bag (c_meta2) - GreaterEq *) 
            (match v2 with
              | Finite s2 ->
                  FloatSet.iter (fun f -> 
                    bounds_to_add := Bags.BoundSet.add (Bags.GreaterEq f) !bounds_to_add
                  ) s2
              | Top -> ()
            );

            (* Collect bounds from left bag (c_meta1) - Greater *) 
            (match v1 with
              | Finite s1 ->
                  FloatSet.iter (fun f -> 
                    bounds_to_add := Bags.BoundSet.add (Bags.Greater f) !bounds_to_add
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

      (TBool, TAExprNode (GreaterEq ((t1,a1), (t2,a2)))) (* Result is TBool *)

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

    | FinGt (e1, e2, n) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinGt modulus: <#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Types.TFin n in
      (try sub_type t1 expected_type
        with Failure msg -> failwith (Printf.sprintf "Type error in FinGt (<#%d) left operand: %s" n msg));
      (try sub_type t2 expected_type
        with Failure msg -> failwith (Printf.sprintf "Type error in FinGt (<#%d) right operand: %s" n msg));
      (Types.TBool, TAExprNode (FinGt ((t1, a1), (t2, a2), n)))
      
    | FinGeq (e1, e2, n) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinGeq modulus: <=#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = Types.TFin n in
      (try sub_type t1 expected_type
        with Failure msg -> failwith (Printf.sprintf "Type error in FinGeq (<=#%d) left operand: %s" n msg));
      (try sub_type t2 expected_type
        with Failure msg -> failwith (Printf.sprintf "Type error in FinGeq (<=#%d) right operand: %s" n msg));
      (Types.TBool, TAExprNode (FinGeq ((t1, a1), (t2, a2), n)))

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

(* Function that does infer but insists that the return type is TBool *)
let infer_bool (e : expr) : texpr =
  let t, a = infer e in
  sub_type t Types.TBool;
  (t, a) 