(**
 * @file interp.ml
 * @brief Interpreter for the ContDice language.
 *
 * This module provides the main evaluation logic for ContDice expressions.
 * It takes an AST (Abstract Syntax Tree) as input and produces a runtime value.
 * The interpreter handles variable lookup, function application, distribution sampling,
 * control flow, and other language constructs. It uses an environment to store
 * variable bindings during evaluation.
 *)
open Types (* To access the sub-modules Ast, RuntimeValues etc. *)
open Ast (* For expr, ExprNode, and AST constructors *)
open RuntimeValues (* For value, env, and value constructors *)

(* Ensure Random is initialized for probabilistic sampling. *)
let () = Random.self_init ()

(** Exception raised for general runtime errors not caught by more specific exceptions. *)
exception RuntimeError of string (* Will be gradually replaced *)
exception TypeError of string
exception UnboundVariableError of string
exception InvalidArgumentsError of string
exception DistributionError of string
exception ObserveFailure (* Custom exception for observe statement failures (condition is false). *)

(**
 * @function lookup
 * @brief Looks up a variable's value in the current environment.
 * The environment is an association list of (string * value) pairs.
 *
 * @param x The name of the variable to look up.
 * @param env The current environment.
 * @return The value associated with variable `x`.
 * @raise UnboundVariableError if the variable `x` is not found in the environment.
 *)
let rec lookup x env =
  match env with
  | [] -> raise (UnboundVariableError ("Unbound variable: " ^ x))
  | (y, v) :: rest -> if x = y then v else lookup x rest

(**
 * @function eval
 * @brief Main evaluation function for ContDice expressions.
 * Recursively evaluates an expression node within a given environment.
 *
 * @param env The current environment for variable lookups.
 * @param e The expression to evaluate (specifically, the `ExprNode` wrapper around an `expr_generic` node).
 * @return The `value` resulting from the evaluation.
 * @raise Various exceptions (TypeError, UnboundVariableError, etc.) on evaluation errors.
 *)
let rec eval (env : env) (ExprNode e_node : expr) : value =
  match e_node with
  (* Basic constants and variables *)
  | Const f -> VFloat f
  | BoolConst b -> VBool b
  | Var x -> lookup x env (* Look up variable in environment *)

  (* Let binding: evaluate e1, bind its result to x in a new environment, then eval e2 *)
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2 (* Add new binding to the front of the environment *)

  (* Sampling from a probability distribution *)
  | Sample dist_exp -> (
      let dist = eval_dist env dist_exp in (* First, evaluate the distribution parameters *)
      (* Attempt to sample from the continuous distribution using the Distributions module. *)
      try VFloat (Distributions.cdistr_sample dist) with
      | Invalid_argument msg -> (* Catch errors from Distributions (e.g., invalid parameters) *)
          raise
            (InvalidArgumentsError
               (Printf.sprintf "Invalid distribution parameters: %s" msg))
      | Failure msg -> (* Catch other potential failures during sampling *)
          raise (DistributionError (Printf.sprintf "Sampling error: %s" msg))
      )

  (* Discrete distribution defined by cases *)
  | DistrCase cases ->
      (* Optional check: Ensure probabilities sum to approximately 1.0.
         This should ideally be caught during static analysis (elaboration) if possible.
         let total_prob = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 cases in
         if abs_float (total_prob -. 1.0) > 0.0001 then
           raise (RuntimeError "DistrCase probabilities do not sum to 1.0");
      *)
      let r = Random.float 1.0 in (* Generate a random float in [0.0, 1.0) *)
      (* Iterate through cases, accumulating probabilities to find the selected case. *)
      let rec find_case cumulative_prob case_list =
        match case_list with
        | [] ->
            (* This should not happen if probabilities sum to 1 and r is in [0,1). *)
            raise
              (RuntimeError
                 "DistrCase: Random value exceeded total probability (should not happen)")
        | (e, p) :: rest ->
            let next_cumulative_prob = cumulative_prob +. p in
            if r <= next_cumulative_prob then eval env e (* If r falls into this case's probability range, evaluate its expression. *)
            else find_case next_cumulative_prob rest (* Otherwise, check the next case. *)
      in
      find_case 0.0 cases

  (* Binary arithmetic/comparison operations *)
  | Less (e1, e2) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match (v1, v2) with
      | VFloat f1, VFloat f2 -> VBool (f1 < f2)
      | _ ->
          raise
            (TypeError "Type error during evaluation: Less expects floats"))
  | LessEq (e1, e2) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match (v1, v2) with
      | VFloat f1, VFloat f2 -> VBool (f1 <= f2)
      | _ ->
          raise
            (TypeError "Type error during evaluation: LessEq expects floats")
      )
  | And (e1, e2) -> (
      let v1 = eval env e1 in
      match v1 with
      | VBool false -> VBool false
      | VBool true -> (
          let v2 = eval env e2 in
          match v2 with
          | VBool b -> VBool b
          | _ ->
              raise
                (TypeError
                   "Type error during evaluation: And (&&) expects booleans"))
      | _ ->
          raise
            (TypeError
               "Type error during evaluation: And (&&) expects booleans"))
  | Or (e1, e2) -> (
      let v1 = eval env e1 in
      match v1 with
      | VBool true -> VBool true
      | VBool false -> (
          let v2 = eval env e2 in
          match v2 with
          | VBool b -> VBool b
          | _ ->
              raise
                (TypeError
                   "Type error during evaluation: Or (||) expects booleans"))
      | _ ->
          raise
            (TypeError
               "Type error during evaluation: Or (||) expects booleans"))
  | Not e1 -> (
      let v1 = eval env e1 in
      match v1 with
      | VBool b -> VBool (not b)
      | _ ->
          raise
            (TypeError "Type error during evaluation: Not expects a boolean")
      )
  | If (e_cond, e_then, e_else) -> (
      let v_cond = eval env e_cond in
      match v_cond with
      | VBool true -> eval env e_then
      | VBool false -> eval env e_else
      | _ ->
          raise
            (TypeError
               "Type error during evaluation: If condition expects a boolean"))
  | Pair (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      VPair (v1, v2)
  | First e -> (
      let v = eval env e in
      match v with
      | VPair (v1, _) -> v1
      | _ ->
          raise
            (TypeError "Type error during evaluation: First expects a pair"))
  | Second e -> (
      let v = eval env e in
      match v with
      | VPair (_, v2) -> v2
      | _ ->
          raise
            (TypeError "Type error during evaluation: Second expects a pair")
      )
  (* Function definition: create a closure capturing the parameter, body, and current environment *)
  | Fun (x, body) -> VClosure (x, body, env) (* Note: `env` is the definition-time environment *)

  (* Function application *)
  | FuncApp (e_fun, e_arg) -> (
      let v_fun = eval env e_fun in (* Evaluate the function expression *)
      let v_arg = eval env e_arg in (* Evaluate the argument expression *)
      match v_fun with
      | VClosure (x, body, captured_env) ->
          (* If it's a closure, evaluate its body in the *captured* environment,
             extended with the new argument binding (x -> v_arg). *)
          eval ((x, v_arg) :: captured_env) body
      | _ -> (* Not a function *)
          raise
            (TypeError
               "Type error during evaluation: Application expects a function"))

  (* Loop application (iterate): Similar to FuncApp for one step, iteration handled by specific constructs or elaboration.
     The `_` for the number of iterations (n) indicates it's not directly used in this basic eval step of a single call.
     A full loop construct would typically be desugared or handled by a different interpreter mechanism if not unrolled. *)
  | LoopApp (e_fun, e_arg, _) -> (
      let v_fun = eval env e_fun in
      let v_arg = eval env e_arg in
      match v_fun with
      | VClosure (x, body, captured_env) ->
          eval ((x, v_arg) :: captured_env) body
      | _ ->
          raise
            (TypeError
               "Type error during evaluation: Loop application expects a function-like closure"))
  | FinConst (k, n) -> VFin (k, n) (* Finite set constant k#n *)
  | FinLt (e1, e2, n) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match (v1, v2) with
      | VFin (k1, n1), VFin (k2, n2) when n1 = n && n2 = n -> VBool (k1 < k2)
      | _ ->
          raise
            (TypeError
               (Printf.sprintf
                  "Type error during evaluation: FinLt expects Fin(%d)" n)))
  | FinLeq (e1, e2, n) -> (
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match (v1, v2) with
      | VFin (k1, n1), VFin (k2, n2) when n1 = n && n2 = n -> VBool (k1 <= k2)
      | _ ->
          raise
            (TypeError
               (Printf.sprintf
                  "Type error during evaluation: FinLeq expects Fin(%d)" n)))
  | FinEq (e1, e2, n) -> (
      (* New case for FinEq *)
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match (v1, v2) with
      | VFin (k1, n1), VFin (k2, n2) when n1 = n && n2 = n -> VBool (k1 = k2)
      | _ ->
          raise
            (TypeError
               (Printf.sprintf
                  "Type error during evaluation: FinEq expects Fin(%d)" n)))
  (* Observe statement: evaluate condition, if false, raise ObserveFailure *)
  | Observe e1 -> (
      let v1 = eval env e1 in
      match v1 with
      | VBool true -> VUnit (* Observation consistent, program continues, returns Unit *)
      | VBool false -> raise ObserveFailure (* Observation inconsistent, specific failure *)
      | _ ->
          raise
            (TypeError
               "Type error during evaluation: Observe expects a boolean condition"))

  (* Fixed-point combinator (for recursion) *)
  | Fix (f, x, body) ->
      (* Create a recursive closure: the closure's environment includes itself (f -> closure_val) *)
      let rec closure_val = VClosure (x, body, (f, closure_val) :: env) in
      closure_val (* The result of Fix is this recursive closure *)

  (* List constructors *)
  | Nil -> VNil (* Empty list *)
  | Cons (e_hd, e_tl) ->
      let v_hd = eval env e_hd in (* Evaluate head expression *)
      let v_tl = eval env e_tl in (* Evaluate tail expression *)
      VCons (v_hd, v_tl) (* Construct list cell *)

  (* List pattern matching *)
  | MatchList (e_match, e_nil, y, ys, e_cons) -> (
      let v_match = eval env e_match in (* Evaluate the list expression to be matched *)
      match v_match with
      | VNil -> eval env e_nil (* If it's Nil, evaluate the nil branch *)
      | VCons (v_hd, v_tl) ->
          (* If it's Cons, bind head (y) and tail (ys) in a new environment and evaluate the cons branch *)
          let env_cons = (y, v_hd) :: (ys, v_tl) :: env in
          eval env_cons e_cons
      | _ -> (* Not a list *)
          raise
            (TypeError
               "Type error during evaluation: MatchList expects a list value"))

  (* Reference operations *)
  | Ref e ->
      let v = eval env e in
      VRef (ref v) (* Create a new OCaml mutable reference holding the evaluated value *)
  | Deref e -> (
      let v = eval env e in
      match v with
      | VRef r -> !r (* Dereference the OCaml reference *)
      | _ -> (* Not a reference *)
          raise
            (TypeError
               "Type error during evaluation: Deref expects a reference value"))
  | Assign (e_ref, e_val) -> (
      let v_ref = eval env e_ref in (* Evaluate the reference expression *)
      let v_val = eval env e_val in (* Evaluate the value expression to assign *)
      match v_ref with
      | VRef r ->
          r := v_val; (* Assign the value to the OCaml reference *)
          VUnit (* Assignment operation returns Unit *)
      | _ -> (* Not a reference on the left-hand side *)
          raise
            (TypeError
               "Type error during evaluation: Assignment expects a reference on the left-hand side"))

  (* Sequencing: evaluate e1 for side effects, discard its result, then evaluate and return e2 *)
  | Seq (e1, e2) ->
      let _ = eval env e1 in (* Evaluate e1, result ignored *)
      eval env e2 (* Evaluate e2, its result is the result of the sequence *)

  | Unit -> VUnit (* Unit literal *)

  (* This case should ideally not be reached if all AST nodes are handled.
     It might be for a RuntimeError variant within the AST.expr_generic type itself,
     which is unusual for an interpreter. *)
  | RuntimeError s -> raise (RuntimeError s)

(**
 * @function eval_dist
 * @brief Evaluates distribution expressions to their runtime representation.
 * This helper function is used by the `Sample` case in the main `eval` function.
 * It takes a distribution kind (e.g., `DUniform`, `DGaussian`) and its argument
 * expressions, evaluates the arguments, and constructs a `Distributions.cdistr` value.
 *
 * @param env The current environment.
 * @param dist_exp The distribution expression (`'a sample` from `Ast.ml`) to evaluate.
 * @return A `Distributions.cdistr` value representing the evaluated distribution.
 * @raise TypeError if distribution arguments are not of the expected type (VFloat).
 *)
and eval_dist env dist_exp =
  match dist_exp with
  | Distr1 (kind, e1) -> ( (* Single-argument distribution *)
      let v1 = eval env e1 in (* Evaluate the argument *)
      match v1 with
      | VFloat f1 -> Distributions.get_cdistr_from_single_arg_kind kind f1 (* Construct cdistr *)
      | _ -> (* Argument not a float *)
          raise
            (TypeError
               "Type error: single-argument distribution expects a float parameter"))
  | Distr2 (kind, e1, e2) -> ( (* Two-argument distribution *)
      let v1 = eval env e1 in (* Evaluate first argument *)
      let v2 = eval env e2 in (* Evaluate second argument *)
      match (v1, v2) with
      | VFloat f1, VFloat f2 ->
          Distributions.get_cdistr_from_two_arg_kind kind f1 f2 (* Construct cdistr *)
      | _, _ -> (* One or both arguments not floats *)
          raise
            (TypeError
               "Type error: two-argument distribution expects float parameters"))

(**
 * @function run
 * @brief Entry point for evaluating a ContDice expression.
 * Initializes an empty environment and calls the main `eval` function.
 *
 * @param e The expression to evaluate.
 * @return The `value` resulting from the evaluation.
 *)
let run e = eval [] e
