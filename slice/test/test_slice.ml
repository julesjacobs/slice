open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Slice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Slice.Inference.infer expr in
    ignore (Slice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

let test_uniform_requires_strict_bounds _test_ctxt =
  match Slice.Distributions.get_cdistr_from_two_arg_kind Slice.Ast.DUniform 0.0 0.0 with
  | Error _ -> ()
  | Ok _ -> assert_failure "uniform(0, 0) should be rejected"

let test_uniform_cdf_is_clamped _test_ctxt =
  let dist = Slice.Distributions.Uniform (0.0, 1.0) in
  let cdf = Slice.Distributions.cdistr_cdf dist in
  assert_equal 0.0 (cdf (-1.0));
  assert_equal 0.0 (cdf 0.0);
  assert_equal 0.25 (cdf 0.25);
  assert_equal 1.0 (cdf 1.0);
  assert_equal 1.0 (cdf 2.0)

let test_uniform_cdf_handles_extreme_finite_bounds _test_ctxt =
  let dist = Slice.Distributions.Uniform (-.max_float, max_float) in
  let cdf = Slice.Distributions.cdistr_cdf dist in
  assert_equal 0.0 (cdf (-.max_float));
  assert_equal 0.5 (cdf 0.0);
  assert_equal 1.0 (cdf max_float)

let test_uniform_requires_finite_bounds _test_ctxt =
  let non_finite_bounds =
    [ (nan, 1.0); (0.0, nan);
      (neg_infinity, 1.0); (infinity, 1.0);
      (0.0, neg_infinity); (0.0, infinity) ]
  in
  List.iter
    (fun (lo, hi) ->
      (match Slice.Distributions.get_cdistr_from_two_arg_kind
               Slice.Ast.DUniform lo hi with
       | Error "Uniform bounds must be finite" -> ()
       | Error msg ->
           assert_failure ("unexpected uniform validation error: " ^ msg)
       | Ok _ -> assert_failure "uniform bounds must be finite");
      let dist = Slice.Distributions.Uniform (lo, hi) in
      let expected = Invalid_argument "Uniform bounds must be finite" in
      assert_raises expected
        (fun () -> ignore (Slice.Distributions.cdistr_cdf dist 0.0));
      assert_raises expected
        (fun () -> ignore (Slice.Distributions.cdistr_sample dist)))
    non_finite_bounds

let test_degenerate_uniform_operations_fail _test_ctxt =
  let dist = Slice.Distributions.Uniform (0.0, 0.0) in
  assert_raises (Invalid_argument "Uniform low must be less than high")
    (fun () -> ignore (Slice.Distributions.cdistr_cdf dist 0.0));
  assert_raises (Invalid_argument "Uniform low must be less than high")
    (fun () -> ignore (Slice.Distributions.cdistr_sample dist))

let test_degenerate_uniform_program_fails _test_ctxt =
  let expr = Slice.Parse.parse_expr "uniform(0, 0) < 0" in
  let expected = Slice.Interp.RuntimeError "Uniform low must be less than high" in
  assert_raises expected (fun () -> ignore (Slice.Interp.run expr));
  let discretized = expr |> Slice.Inference.infer |> Slice.Discretization.discretize in
  assert_raises expected (fun () -> ignore (Slice.Interp.run discretized))

(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "uniform_requires_strict_bounds" >:: test_uniform_requires_strict_bounds;
     "uniform_cdf_is_clamped" >:: test_uniform_cdf_is_clamped;
     "uniform_cdf_handles_extreme_finite_bounds" >:: test_uniform_cdf_handles_extreme_finite_bounds;
     "uniform_requires_finite_bounds" >:: test_uniform_requires_finite_bounds;
     "degenerate_uniform_operations_fail" >:: test_degenerate_uniform_operations_fail;
     "degenerate_uniform_program_fails" >:: test_degenerate_uniform_program_fails;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
