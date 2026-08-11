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
    "model ~= weibull_min(c=3, scale=2)"

let test_sppl_pareto _test_ctxt =
  assert_sppl "pareto(2, 3)"
    "model ~= pareto(b=2, scale=3)"

let test_sppl_gumbel1 _test_ctxt =
  assert_sppl "gumbel1(2, 4)"
    "model ~= gumbel_r(loc=0.69314718055994529, scale=0.5)"

let test_sppl_gumbel2 _test_ctxt =
  assert_sppl "gumbel2(2, 4)"
    "model ~= invweibull(c=2, scale=2)"

let test_sppl_exppow _test_ctxt =
  assert_sppl "exppow(2, 3)"
    "model ~= gennorm(beta=3, scale=2)"

let test_sppl_binomial _test_ctxt =
  assert_sppl "binomial(0.25, 8)"
    "model ~= binom(n=8, p=0.25)"

let test_sppl_scipy_names_and_parameters _test_ctxt =
  assert_sppl "gaussian(0.12345678901234566, 1.2345678901234567)"
    "model ~= norm(loc=0.12345678901234566, scale=1.2345678901234567)";
  assert_sppl "exponential(3)"
    "model ~= expon(scale=0.33333333333333331)";
  assert_sppl "lognormal(0.6931471805599453, 0.12345678901234566)"
    "model ~= lognorm(s=0.12345678901234566, scale=2)";
  assert_sppl "gamma(1.2345678901234567, 2.3456789012345678)"
    "model ~= gamma(a=1.2345678901234567, scale=2.3456789012345678)"

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
     "test_sppl_scipy_names_and_parameters" >:: test_sppl_scipy_names_and_parameters;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
