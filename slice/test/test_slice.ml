open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Slice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Slice.Inference.infer expr in
    ignore (Slice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

let assert_sppl source expected =
  let expr = Slice.Parse.parse_expr source in
  assert_equal ~printer:(fun x -> x) expected
    (Slice.Pretty.slice_expr_to_sppl_prog expr)

let test_sppl_weibull _test_ctxt =
  assert_sppl "weibull(2, 3)"
    "model ~= weibull_min(c=3.000000, scale=2.000000)"

let test_sppl_pareto _test_ctxt =
  assert_sppl "pareto(2, 3)"
    "model ~= pareto(b=2.000000, scale=3.000000)"

let test_sppl_gumbel1 _test_ctxt =
  assert_sppl "gumbel1(2, 4)"
    "model ~= gumbel_r(loc=0.693147, scale=0.500000)"

let test_sppl_gumbel2 _test_ctxt =
  assert_sppl "gumbel2(2, 4)"
    "model ~= invweibull(c=2.000000, scale=2.000000)"

let test_sppl_exppow _test_ctxt =
  assert_sppl "exppow(2, 3)"
    "model ~= gennorm(beta=3.000000, scale=2.000000)"

let test_sppl_binomial _test_ctxt =
  assert_sppl "binomial(0.25, 8)"
    "model ~= binom(n=8.000000, p=0.250000)"

(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "test_sppl_weibull" >:: test_sppl_weibull;
     "test_sppl_pareto" >:: test_sppl_pareto;
     "test_sppl_gumbel1" >:: test_sppl_gumbel1;
     "test_sppl_gumbel2" >:: test_sppl_gumbel2;
     "test_sppl_exppow" >:: test_sppl_exppow;
     "test_sppl_binomial" >:: test_sppl_binomial;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
