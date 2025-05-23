(*
This module implements the interpreter (evaluator) for the language's 
expressions (`expr` type from `Types`). It recursively walks the AST,
evaluating expressions in the context of an environment that maps variable
names to their current values (`value` type).
It handles basic constants, arithmetic/boolean operations, control flow,
function definition and application, sampling from distributions, and
probabilistic constructs like `Observe` and `DistrCase`.
*)
open Types

(* Ensures the OCaml standard library's random number generator is initialized.
   This is crucial for `Random.float` used in `DistrCase`. *)
let () = Random.self_init ()

(** Custom exception raised for various errors encountered during evaluation,
    such as unbound variables, type mismatches during runtime checks, or
    errors from distribution parameter validation. *)
exception RuntimeError of string
(** Custom exception raised specifically when an `(observe false)` statement
    is evaluated. This typically signals a point of zero probability in the
    program's execution trace. It's expected to be caught by a higher-level
    sampling algorithm (e.g., for rejection sampling or importance weighting)
    rather than being an internal interpreter error. *)
exception ObserveFailure

(** [lookup x env] retrieves the value of variable [x] from the environment [env].
    The environment is a list of (string * value) pairs.
    Raises [RuntimeError] if the variable is not found (unbound). *)
let rec lookup x env = 
  match env with
  | [] -> raise (RuntimeError ("Unbound variable: " ^ x))
  | (y, v)::rest -> if x = y then v else lookup x rest

(** [eval env e_node] is the main recursive function that evaluates an expression
    node [e_node] in the context of the current environment [env].
    It returns a [value] representing the result of the evaluation. *)
let rec eval (env : env) (ExprNode e_node : expr) : value = 
  match e_node with
  | Const f -> VFloat f (* Float constant evaluates to VFloat *)
  | BoolConst b -> VBool b (* Boolean constant evaluates to VBool *)
  | Var x -> lookup x env (* Variable evaluates by looking up in environment *)

  | Let (x, e1, e2) ->
      (* Evaluate definition e1, then evaluate e2 in environment extended with x bound to v1 *)
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2

  | Sample dist_exp -> 
      (* Evaluate the distribution expression to get a concrete distribution `dist` *)
      let dist = eval_dist env dist_exp in
      (* Sample from the continuous distribution.
         Catches potential errors from invalid distribution parameters or unimplemented cases. *)
      begin
        try
          VFloat (Distributions.cdistr_sample dist)
        with 
        | Invalid_argument msg -> raise (RuntimeError (Printf.sprintf "Invalid distribution parameters for sampling: %s" msg))
        | Failure msg -> raise (RuntimeError (Printf.sprintf "Sampling error from distribution: %s" msg))
      end

  | DistrCase cases ->
      (* The sum of probabilities in `cases` is assumed to be 1.0 (checked during elaboration).
         Generate a random float `r` in [0, 1.0).
         Iterate through cases, accumulating probability, and evaluate the expression
         of the first case where `r` falls within its cumulative probability range. *)
      let r = Random.float 1.0 in
      let rec find_case cumulative_prob case_list =
        match case_list with
        | [] -> raise (RuntimeError "DistrCase: Random value exceeded total probability or empty cases (should not happen if validated)")
        | (e, p) :: rest -> 
            let next_cumulative_prob = cumulative_prob +. p in
            if r <= next_cumulative_prob then eval env e
            else find_case next_cumulative_prob rest
      in
      find_case 0.0 cases

  | Cmp (cmp_op, e1, e2, _flipped) ->
      (* Evaluate both operands to values *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VFloat f1, VFloat f2 ->
           (* Perform comparison based on cmp_op *)
           let result = match cmp_op with
             | Types.Lt -> f1 < f2
             | Types.Le -> f1 <= f2  
           in
           VBool result
       | _ -> (* Runtime type error if operands are not VFloat *)
           let op_name = match cmp_op with Types.Lt -> "Less" | Types.Le -> "LessEq" in
           raise (RuntimeError ("Type error during evaluation: " ^ op_name ^ " expects float operands")))

  | FinCmp (cmp_op, e1, e2, n, _flipped) ->
      (* Evaluate both operands *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VFin (k1, n1), VFin (k2, n2) when n1 = n && n2 = n ->
           (* Perform comparison on k values if moduli match *)
           let result = match cmp_op with
             | Types.Lt -> k1 < k2
             | Types.Le -> k1 <= k2
           in
           VBool result
       | _ -> (* Runtime type error if not VFin with correct modulus *)
           let op_name = match cmp_op with Types.Lt -> "FinLt" | Types.Le -> "FinLeq" in
           raise (RuntimeError (Printf.sprintf "Type error during evaluation: FinCmp %s expects Fin(%d) operands" op_name n)))

  | And (e1, e2) ->
      (* Short-circuiting evaluation for And *)
      let v1 = eval env e1 in
      (match v1 with
       | VBool false -> VBool false (* If e1 is false, result is false *)
       | VBool true ->  (* If e1 is true, result is evaluation of e2 *)
           let v2 = eval env e2 in
           (match v2 with
            | VBool b -> VBool b
            | _ -> raise (RuntimeError "Type error during evaluation: And (&&) expects boolean operands"))
       | _ -> raise (RuntimeError "Type error during evaluation: And (&&) expects boolean operands"))

  | Or (e1, e2) ->
      (* Short-circuiting evaluation for Or *)
      let v1 = eval env e1 in
      (match v1 with
       | VBool true -> VBool true (* If e1 is true, result is true *)
       | VBool false -> (* If e1 is false, result is evaluation of e2 *)
           let v2 = eval env e2 in
           (match v2 with
            | VBool b -> VBool b
            | _ -> raise (RuntimeError "Type error during evaluation: Or (||) expects boolean operands"))
       | _ -> raise (RuntimeError "Type error during evaluation: Or (||) expects boolean operands"))

  | Not e1 ->
      (* Evaluate operand, then negate if it's a boolean *)
      let v1 = eval env e1 in
      (match v1 with
       | VBool b -> VBool (not b)
       | _ -> raise (RuntimeError "Type error during evaluation: Not expects a boolean operand"))

  | If (e_cond, e_then, e_else) ->
      (* Evaluate condition, then evaluate either then-branch or else-branch *)
      let v_cond = eval env e_cond in
      (match v_cond with
       | VBool true -> eval env e_then
       | VBool false -> eval env e_else
       | _ -> raise (RuntimeError "Type error during evaluation: If condition expects a boolean"))

  | Pair (e1, e2) ->
      (* Evaluate both expressions and form a VPair value *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      VPair (v1, v2)

  | First e ->
      (* Evaluate expression e, expect VPair, return its first component *)
      let v = eval env e in
      (match v with
       | VPair (v1, _) -> v1
       | _ -> raise (RuntimeError "Type error during evaluation: First (fst) expects a pair operand"))

  | Second e ->
      (* Evaluate expression e, expect VPair, return its second component *)
      let v = eval env e in
      (match v with
       | VPair (_, v2) -> v2
       | _ -> raise (RuntimeError "Type error during evaluation: Second (snd) expects a pair operand"))

  | Fun (x, body) ->
      (* Create a VClosure, capturing the current environment `env` along with parameter `x` and body `body`. *)
      VClosure (x, body, env)

  | FuncApp (e_fun, e_arg) ->
      (* Evaluate function expression `e_fun` (expect VClosure) and argument `e_arg`.
         Then, evaluate the closure's body in its captured environment, extended with
         the parameter `x` bound to the argument's value `v_arg`. *)
      let v_fun = eval env e_fun in
      let v_arg = eval env e_arg in
      (match v_fun with
       | VClosure (x, body, captured_env) ->
           eval ((x, v_arg) :: captured_env) body
       | _ -> raise (RuntimeError "Type error during evaluation: Application expects a function value"))

  | LoopApp (e_fun, e_arg, _) -> (* Loop count is not used in this interpreter, similar to FuncApp *)
        let v_fun = eval env e_fun in
        let v_arg = eval env e_arg in
        (match v_fun with
            | VClosure (x, body, captured_env) ->
                eval ((x, v_arg) :: captured_env) body
            | _ -> raise (RuntimeError "Type error during evaluation: Loop Application expects a function value"))
    
  | FinConst (k, n) -> VFin (k, n) (* Finite constant evaluates to VFin *)

  | FinEq (e1, e2, n) ->
      (* Evaluate both operands, expect VFin with matching modulus n, compare k values. *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VFin (k1, n1), VFin (k2, n2) when n1 = n && n2 = n -> VBool (k1 = k2)
       | _ -> raise (RuntimeError (Printf.sprintf "Type error during evaluation: FinEq expects Fin(%d) operands" n)))

  | Observe e1 -> 
      (* Evaluate the boolean expression e1.
         If true, the observation is consistent, return VUnit.
         If false, raise ObserveFailure, signaling an impossible path. *)
      let v1 = eval env e1 in
      (match v1 with
       | VBool true -> VUnit 
       | VBool false -> raise ObserveFailure
       | _ -> raise (RuntimeError "Type error during evaluation: Observe expects a boolean operand"))

  | Fix (f, x, body) -> 
      (* Create a recursive closure for `Fix f x -> body`.
         The closure's captured environment includes `f` bound to the closure itself (`closure_val`). *)
      let rec closure_val = VClosure (x, body, (f, closure_val) :: env) in 
      closure_val

  | Nil -> VNil (* Nil list evaluates to VNil *)

  | Cons (e_hd, e_tl) -> 
      (* Evaluate head and tail, then construct VCons value *)
      let v_hd = eval env e_hd in
      let v_tl = eval env e_tl in
      VCons (v_hd, v_tl)

  | MatchList (e_match, e_nil, y, ys, e_cons) ->
      (* Evaluate the list to be matched.
         If VNil, evaluate the nil-branch `e_nil`.
         If VCons, evaluate the cons-branch `e_cons` in an environment extended
         with `y` (head) and `ys` (tail) bindings. *)
      let v_match = eval env e_match in
      (match v_match with
       | VNil -> eval env e_nil
       | VCons (v_hd, v_tl) -> 
           let env_cons = (y, v_hd) :: (ys, v_tl) :: env in
           eval env_cons e_cons
       | _ -> raise (RuntimeError "Type error during evaluation: MatchList expects a list value"))

  | Ref e -> 
      (* Evaluate expression `e`, then create an OCaml reference to its value, wrapped in VRef. *)
      let v = eval env e in
      VRef (ref v)

  | Deref e ->
      (* Evaluate expression `e` (expect VRef), then dereference the OCaml reference. *)
      let v = eval env e in
      (match v with
       | VRef r -> !r 
       | _ -> raise (RuntimeError "Type error during evaluation: Deref expects a reference value"))

  | Assign (e_ref, e_val) ->
      (* Evaluate `e_ref` (expect VRef) and `e_val`.
         Assign the value of `e_val` to the OCaml reference in `e_ref`. Returns VUnit. *)
      let v_ref = eval env e_ref in
      let v_val = eval env e_val in
      (match v_ref with
       | VRef r -> 
           r := v_val; 
           VUnit       
       | _ -> raise (RuntimeError "Type error during evaluation: Assignment expects a reference value on the left"))

  | Seq (e1, e2) ->
      (* Evaluate `e1` for its side effects, discard its result.
         Then, evaluate `e2` and return its result. *)
      let _ = eval env e1 in 
      eval env e2

  | Unit -> VUnit (* Unit expression evaluates to VUnit *)

  | RuntimeError s -> raise (RuntimeError s) (* Propagate RuntimeError nodes as exceptions *)

(** [eval_dist env dist_exp] evaluates a distribution expression [dist_exp]
    (which is part of a `Sample` node) in the given environment [env].
    It evaluates the parameters of the distribution, then uses helper functions
    from the `Distributions` module (like `get_cdistr_from_single_arg_kind`)
    to construct a concrete `Distributions.cdistr` value.
    Raises [RuntimeError] if parameters are not floats or are invalid for the distribution.
*)
and eval_dist env dist_exp = 
  match dist_exp with
  | Distr1 (kind, e1) ->
      let v1 = eval env e1 in
      (match v1 with
       | VFloat f1 -> 
           (match Distributions.get_cdistr_from_single_arg_kind kind f1 with
            | Ok dist -> dist
            | Error msg -> raise (RuntimeError ("Distribution parameter error: " ^ msg)))
       | _ -> raise (RuntimeError "Type error: single-argument distribution expects a VFloat parameter"))
  | Distr2 (kind, e1, e2) -> 
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1, v2 with
       | VFloat f1, VFloat f2 -> 
           (match Distributions.get_cdistr_from_two_arg_kind kind f1 f2 with
            | Ok dist -> dist
            | Error msg -> raise (RuntimeError ("Distribution parameter error: " ^ msg)))
       | _ -> raise (RuntimeError "Type error: two-argument distribution expects VFloat parameters"))

(** [run e] is the main entry point for evaluating an expression [e].
    It calls `eval` with an initially empty environment `[]`. *)
let run e = eval [] e 