open OUnit2

(* Simple test for parse_expr *)
let test_parse _test_ctxt =
  let expr = Contdice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  let pretty = Contdice.Pretty.string_of_expr expr in
  let expected = "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  assert_equal ~printer:(fun s -> s) expected pretty

(* Test for pretty printing *)
let test_pretty_print _test_ctxt =
  let expr = Contdice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  let pretty = Contdice.Pretty.string_of_expr expr in
  let expected = "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  assert_equal ~printer:(fun s -> s) expected pretty

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Contdice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Contdice.elab expr in
    ignore (Contdice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

(* Test suite *) 
let suite = 
  "Contdice Tests" >:::
    ["test_parse" >:: test_parse;
     "test_pretty_print" >:: test_pretty_print;
     "test_typing" >:: test_typing;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite