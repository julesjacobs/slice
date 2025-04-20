(* Simple test for parse_expr *)
let test_parse () =
  let expr = Contdice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  let pretty = Contdice.Pretty.string_of_expr expr in
  let expected = "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  
  Printf.printf "Parsed: %s\n" pretty;
  if pretty = expected then
    Printf.printf "Parse test passed!\n"
  else begin
    Printf.printf "Parse test failed!\nExpected: %s\nGot:      %s\n" expected pretty;
    exit 1
  end

(* Test for pretty printing *)
let test_pretty_print () =
  let expr = Contdice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  let pretty = Contdice.Pretty.string_of_expr expr in
  let expected = "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  if pretty = expected then
    Printf.printf "Pretty print test passed!\n"
  else begin
    Printf.printf "Pretty print test failed!\nExpected: %s\nGot: %s\n" expected pretty;
    exit 1
  end

(* Test typechecking *)
let test_typing () =
  let expr = Contdice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Contdice.elab expr in
    let pretty = Contdice.Pretty.string_of_texpr texpr in
    Printf.printf "Typed expression: %s\n" pretty;
    Printf.printf "Type check test passed!\n"
  with e -> 
    Printf.printf "Type check test failed: %s\n" (Printexc.to_string e);
    exit 1

(* Main test function *)
let () =
  test_parse ();
  test_pretty_print ();
  test_typing ();
  Printf.printf "All tests passed!\n"