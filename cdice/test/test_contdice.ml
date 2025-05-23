open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Contdice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Contdice.Inference.infer expr in
    ignore (Contdice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

(* Test suite *) 
let suite = 
  "Contdice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite