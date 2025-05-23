open Types

(* Ensure Random is initialized *)
let () = Random.self_init ()

exception RuntimeError of string
exception ObserveFailure (* New custom exception for observe failures *)

(* Look up a variable in the environment *)
let rec lookup x env = 
  match env with
  | [] -> raise (RuntimeError ("Unbound variable: " ^ x))
  | (y, v)::rest -> if x = y then v else lookup x rest

(* Main evaluation function *)
let rec eval (env : env) (ExprNode e_node : expr) : value = 
  match e_node with
  | Const f -> VFloat f
  | BoolConst b -> VBool b
  | Var x -> lookup x env

  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2

  | Sample dist_exp -> 
      let dist = eval_dist env dist_exp in
      (* Handle continuous distributions by sampling using the stats library function *)
      begin
        try
          VFloat (Distributions.cdistr_sample dist)
        with 
        | Invalid_argument msg -> raise (RuntimeError (Printf.sprintf "Invalid distribution parameters: %s" msg))
        | Failure msg -> raise (RuntimeError (Printf.sprintf "Sampling error: %s" msg)) (* Catch unimplemented cases *)
      end

  | DistrCase cases ->
      (* Check probabilities sum close to 1.0 (optional, should be checked by elab) *)
      (* let total_prob = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 cases in *)
      (* if abs_float (total_prob -. 1.0) > 0.0001 then 
         raise (RuntimeError "DistrCase probabilities do not sum to 1.0"); *)
      
      let r = Random.float 1.0 in
      let rec find_case cumulative_prob case_list =
        match case_list with
        | [] -> raise (RuntimeError "DistrCase: Random value exceeded total probability (should not happen)")
        | (e, p) :: rest -> 
            let next_cumulative_prob = cumulative_prob +. p in
            if r <= next_cumulative_prob then eval env e
            else find_case next_cumulative_prob rest
      in
      find_case 0.0 cases

  | Cmp (cmp_op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VFloat f1, VFloat f2 ->
           let result = match cmp_op with
             | Types.Lt -> f1 < f2
             | Types.Le -> f1 <= f2  
             | Types.Gt -> f1 > f2
             | Types.Ge -> f1 >= f2
           in
           VBool result
       | _ -> 
           let op_name = match cmp_op with
             | Types.Lt -> "Less"
             | Types.Le -> "LessEq"
             | Types.Gt -> "Greater" 
             | Types.Ge -> "GreaterEq"
           in
           raise (RuntimeError ("Type error during evaluation: " ^ op_name ^ " expects floats")))

  | FinCmp (cmp_op, e1, e2, n) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VFin (k1, n1), VFin (k2, n2) when n1 = n && n2 = n ->
           let result = match cmp_op with
             | Types.Lt -> k1 < k2
             | Types.Le -> k1 <= k2
             | Types.Gt -> k1 > k2  
             | Types.Ge -> k1 >= k2
           in
           VBool result
       | _ ->
           let op_name = match cmp_op with
             | Types.Lt -> "FinLt"
             | Types.Le -> "FinLeq"
             | Types.Gt -> "FinGt"
             | Types.Ge -> "FinGeq" 
           in
           raise (RuntimeError (Printf.sprintf "Type error during evaluation: FinCmp %s expects Fin(%d)" op_name n)))

  | And (e1, e2) ->
      let v1 = eval env e1 in
      (match v1 with
       | VBool false -> VBool false
       | VBool true -> 
           let v2 = eval env e2 in
           (match v2 with
            | VBool b -> VBool b
            | _ -> raise (RuntimeError "Type error during evaluation: And (&&) expects booleans"))
       | _ -> raise (RuntimeError "Type error during evaluation: And (&&) expects booleans"))

  | Or (e1, e2) ->
      let v1 = eval env e1 in
      (match v1 with
       | VBool true -> VBool true
       | VBool false -> 
           let v2 = eval env e2 in
           (match v2 with
            | VBool b -> VBool b
            | _ -> raise (RuntimeError "Type error during evaluation: Or (||) expects booleans"))
       | _ -> raise (RuntimeError "Type error during evaluation: Or (||) expects booleans"))

  | Not e1 ->
      let v1 = eval env e1 in
      (match v1 with
       | VBool b -> VBool (not b)
       | _ -> raise (RuntimeError "Type error during evaluation: Not expects a boolean"))

  | If (e_cond, e_then, e_else) ->
      let v_cond = eval env e_cond in
      (match v_cond with
       | VBool true -> eval env e_then
       | VBool false -> eval env e_else
       | _ -> raise (RuntimeError "Type error during evaluation: If condition expects a boolean"))

  | Pair (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      VPair (v1, v2)

  | First e ->
      let v = eval env e in
      (match v with
       | VPair (v1, _) -> v1
       | _ -> raise (RuntimeError "Type error during evaluation: First expects a pair"))

  | Second e ->
      let v = eval env e in
      (match v with
       | VPair (_, v2) -> v2
       | _ -> raise (RuntimeError "Type error during evaluation: Second expects a pair"))

  | Fun (x, body) ->
      VClosure (x, body, env) (* Capture current environment *)

  | FuncApp (e_fun, e_arg) ->
      let v_fun = eval env e_fun in
      let v_arg = eval env e_arg in
      (match v_fun with
       | VClosure (x, body, captured_env) ->
           eval ((x, v_arg) :: captured_env) body (* Use captured env, add arg binding *)
       | _ -> raise (RuntimeError "Type error during evaluation: Application expects a function"))

  | LoopApp (e_fun, e_arg, _) ->
        let v_fun = eval env e_fun in
        let v_arg = eval env e_arg in
        (match v_fun with
            | VClosure (x, body, captured_env) ->
                eval ((x, v_arg) :: captured_env) body (* Use captured env, add arg binding *)
            | _ -> raise (RuntimeError "Type error during evaluation: Application expects a loop"))
    
  | FinConst (k, n) -> VFin (k, n)

  | FinEq (e1, e2, n) -> (* New case for FinEq *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VFin (k1, n1), VFin (k2, n2) when n1 = n && n2 = n -> VBool (k1 = k2)
       | _ -> raise (RuntimeError (Printf.sprintf "Type error during evaluation: FinEq expects Fin(%d)" n)))

  | Observe e1 -> 
      let v1 = eval env e1 in
      (match v1 with
       | VBool true -> VUnit (* Observation consistent, return Unit *)
       | VBool false -> raise ObserveFailure (* Raise custom exception *)
       | _ -> raise (RuntimeError "Type error during evaluation: Observe expects a boolean"))

  | Fix (f, x, body) -> 
      let rec closure_val = VClosure (x, body, (f, closure_val) :: env) in 
      closure_val

  | Nil -> VNil

  | Cons (e_hd, e_tl) -> 
      let v_hd = eval env e_hd in
      let v_tl = eval env e_tl in
      VCons (v_hd, v_tl)

  | MatchList (e_match, e_nil, y, ys, e_cons) ->
      let v_match = eval env e_match in
      (match v_match with
       | VNil -> eval env e_nil
       | VCons (v_hd, v_tl) -> 
           let env_cons = (y, v_hd) :: (ys, v_tl) :: env in
           eval env_cons e_cons
       | _ -> raise (RuntimeError "Type error during evaluation: MatchList expects a list"))

  | Ref e -> 
      let v = eval env e in
      VRef (ref v) (* Create a new OCaml reference *)

  | Deref e ->
      let v = eval env e in
      (match v with
       | VRef r -> !r (* Dereference the OCaml reference *)
       | _ -> raise (RuntimeError "Type error during evaluation: Deref expects a reference"))

  | Assign (e_ref, e_val) ->
      let v_ref = eval env e_ref in
      let v_val = eval env e_val in
      (match v_ref with
       | VRef r -> 
           r := v_val; (* Assign using OCaml reference assignment *)
           VUnit       (* Assignment returns unit *)
       | _ -> raise (RuntimeError "Type error during evaluation: Assignment expects a reference on the left"))

  | Seq (e1, e2) ->
      let _ = eval env e1 in (* Evaluate e1 for side effects, discard result *)
      eval env e2 (* Evaluate e2 and return its result *)

  | Unit -> VUnit

  | RuntimeError s -> raise (RuntimeError s)

and eval_dist env dist_exp = 
  match dist_exp with
  | Distr1 (kind, e1) ->
      let v1 = eval env e1 in
      (match v1 with
       | VFloat f1 -> 
           (match Distributions.get_cdistr_from_single_arg_kind kind f1 with
            | Ok dist -> dist
            | Error msg -> raise (RuntimeError msg))
       | _ -> raise (RuntimeError "Type error: single-argument distribution expects a float for cdistr conversion"))
  | Distr2 (kind, e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VFloat f1, VFloat f2 -> 
           (match Distributions.get_cdistr_from_two_arg_kind kind f1 f2 with
            | Ok dist -> dist
            | Error msg -> raise (RuntimeError msg))
       | _ -> raise (RuntimeError "Type error: two-argument distribution expects floats for cdistr conversion"))

(* Entry point for evaluation with an empty environment *)
let run e = eval [] e 