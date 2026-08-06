open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Slice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Slice.Inference.infer expr in
    ignore (Slice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

let test_runtime_error_is_not_successful_return _test_ctxt =
  let open Slice in
  let mc = To_mc.to_mc (Ast.ExprNode (Ast.RuntimeError "bad distribution")) in
  let error_id =
    match mc.error with
    | Some id -> id
    | None -> assert_failure "runtime error did not allocate an error sink"
  in
  assert_bool "runtime error reaches the error sink"
    (List.exists
       (fun transition ->
         transition.To_mc.src = 0 && transition.dst = error_id
         && transition.prob = 1.0)
       mc.transitions);
  assert_bool "runtime error must not reach the successful-return sink"
    (not
       (List.exists
          (fun transition ->
            transition.To_mc.src = 0 && transition.dst = mc.sink)
          mc.transitions))

let test_unevaluated_function_body_is_not_runtime_error _test_ctxt =
  let open Slice in
  let returned_function =
    Ast.ExprNode
      (Ast.Fun ("x", Ast.ExprNode (Ast.RuntimeError "unevaluated body")))
  in
  let mc = To_mc.to_mc returned_function in
  assert_equal None mc.error;
  assert_bool "a returned function reaches the successful-return sink"
    (List.exists
       (fun transition ->
         transition.To_mc.src = 0 && transition.dst = mc.sink
         && transition.prob = 1.0)
       mc.transitions)

let has_edge mc ~src ~dst ~prob =
  List.exists
    (fun transition ->
      transition.Slice.To_mc.src = src && transition.dst = dst
      && abs_float (transition.prob -. prob) < 1e-12)
    mc.Slice.To_mc.transitions

let state_id mc predicate =
  match
    Hashtbl.fold
      (fun id expr found -> if predicate expr then Some id else found)
      mc.Slice.To_mc.expr_of None
  with
  | Some id -> id
  | None -> assert_failure "expected Markov-chain state was not generated"

let diverging_expr () =
  let open Slice.Ast in
  let self_call = ExprNode (FuncApp (ExprNode (Var "loop"), ExprNode Unit)) in
  ExprNode (FuncApp (ExprNode (Fix ("loop", "ignored", self_call)), ExprNode Unit))

let test_success_failure_error_and_divergence_are_disjoint _test_ctxt =
  let open Slice in
  let open Ast in
  let program =
    ExprNode
      (DistrCase
         [ (ExprNode (BoolConst true), 0.125);
           (ExprNode (BoolConst false), 0.125);
           (ExprNode (RuntimeError "bad"), 0.25);
           (ExprNode (Observe (ExprNode (BoolConst false))), 0.25);
           (diverging_expr (), 0.25) ])
  in
  let mc = To_mc.to_mc program in
  let fail_id = Option.get mc.fail in
  let error_id = Option.get mc.error in
  assert_bool "done, fail, and error sinks must be distinct"
    (mc.sink <> fail_id && mc.sink <> error_id && fail_id <> error_id);
  let true_id =
    state_id mc (function ExprNode (BoolConst true) -> true | _ -> false)
  in
  let false_id =
    state_id mc (function ExprNode (BoolConst false) -> true | _ -> false)
  in
  let runtime_error_id =
    state_id mc (function ExprNode (RuntimeError _) -> true | _ -> false)
  in
  let observe_failure_id =
    state_id mc
      (function
        | ExprNode (Observe (ExprNode (BoolConst false))) -> true
        | _ -> false)
  in
  assert_bool "true is a successful return" (has_edge mc ~src:true_id ~dst:mc.sink ~prob:1.0);
  assert_bool "false is a successful return" (has_edge mc ~src:false_id ~dst:mc.sink ~prob:1.0);
  assert_bool "runtime error reaches only error" (has_edge mc ~src:runtime_error_id ~dst:error_id ~prob:1.0);
  assert_bool "failed observation reaches only fail" (has_edge mc ~src:observe_failure_id ~dst:fail_id ~prob:1.0);
  let divergent_self_loop =
    List.exists
      (fun transition ->
        transition.To_mc.src = transition.dst
        && transition.src <> mc.sink && transition.src <> fail_id
        && transition.src <> error_id)
      mc.transitions
  in
  assert_bool "divergence remains outside every terminal sink" divergent_self_loop

let test_zero_successful_return_mass _test_ctxt =
  let open Slice in
  let open Ast in
  let mc =
    To_mc.to_mc
      (ExprNode
         (DistrCase
            [ (ExprNode (RuntimeError "bad"), 0.5);
              (diverging_expr (), 0.5) ]))
  in
  assert_bool "done is unreachable when every path errors or diverges"
    (not
       (List.exists
          (fun transition ->
            transition.To_mc.dst = mc.sink && transition.src <> mc.sink)
          mc.transitions))

let test_non_error_stuck_state_is_rejected _test_ctxt =
  assert_raises
    (Failure "to_mc: stuck on Var (did you forget to substitute?)")
    (fun () ->
      ignore
        (Slice.To_mc.to_mc
           (Slice.Ast.ExprNode (Slice.Ast.Var "unbound"))))

(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "test_runtime_error_is_not_successful_return" >:: test_runtime_error_is_not_successful_return;
     "test_unevaluated_function_body_is_not_runtime_error" >:: test_unevaluated_function_body_is_not_runtime_error;
     "test_success_failure_error_and_divergence_are_disjoint" >:: test_success_failure_error_and_divergence_are_disjoint;
     "test_zero_successful_return_mass" >:: test_zero_successful_return_mass;
     "test_non_error_stuck_state_is_rejected" >:: test_non_error_stuck_state_is_rejected;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
