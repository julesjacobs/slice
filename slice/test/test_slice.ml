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

(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "test_runtime_error_is_not_successful_return" >:: test_runtime_error_is_not_successful_return;
     "test_unevaluated_function_body_is_not_runtime_error" >:: test_unevaluated_function_body_is_not_runtime_error;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
