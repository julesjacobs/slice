open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Slice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Slice.Inference.infer expr in
    ignore (Slice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

let assert_float_list expected actual =
  assert_equal (List.length expected) (List.length actual);
  List.iter2
    (fun expected actual ->
      assert_bool
        (Printf.sprintf "expected %.12g, got %.12g" expected actual)
        (abs_float (expected -. actual) < 1e-10))
    expected actual

let coincident_cuts =
  Slice.Lats.CutSet.of_list
    [Slice.Lats.LessEq 1.0; Slice.Lats.Less 1.0]

let test_canonical_coincident_cut_order _test_ctxt =
  assert_equal
    [Slice.Lats.Less 1.0; Slice.Lats.LessEq 1.0]
    (Slice.Lats.CutSet.elements coincident_cuts)

let test_coincident_cut_indices _test_ctxt =
  let index value =
    fst (Slice.Discretization.get_const_idx_and_modulus value coincident_cuts)
  in
  assert_equal 0 (index 0.5);
  assert_equal 1 (index 1.0);
  assert_equal 2 (index 1.5)

let test_coincident_cut_atomic_masses _test_ctxt =
  let cuts = Slice.Lats.CutSet.elements coincident_cuts in
  let probs =
    Slice.Discretization.interval_probabilities cuts
      (Slice.Distributions.Binomial (0.5, 2))
  in
  assert_float_list [0.25; 0.5; 0.25] probs

let test_coincident_cut_continuous_masses _test_ctxt =
  let cuts = Slice.Lats.CutSet.elements coincident_cuts in
  let probs =
    Slice.Discretization.interval_probabilities cuts
      (Slice.Distributions.Uniform (0.0, 2.0))
  in
  assert_float_list [0.5; 0.0; 0.5] probs

let test_coincident_cut_degenerate_uniform_masses _test_ctxt =
  let cuts = Slice.Lats.CutSet.elements coincident_cuts in
  let probs =
    Slice.Discretization.interval_probabilities cuts
      (Slice.Distributions.Uniform (1.0, 1.0))
  in
  assert_float_list [0.0; 1.0; 0.0] probs

let test_integer_cdf_threshold_safety _test_ctxt =
  let open Slice.Distributions in
  let nan = 0.0 /. 0.0 in
  assert_raises (Invalid_argument "CDF is undefined at NaN")
    (fun () -> ignore (cdistr_cdf (Poisson 1.0) nan));
  assert_raises (Invalid_argument "CDF is undefined at NaN")
    (fun () -> ignore (cdistr_cdf_below (Binomial (0.5, 2)) nan));
  assert_equal 0.0 (cdistr_cdf (Poisson 1.0) (-.max_float));
  assert_raises
    (Invalid_argument "integer CDF boundary is outside the GSL unsigned-int range")
    (fun () -> ignore (cdistr_cdf (Poisson 1.0) max_float));
  assert_equal 0.0 (cdistr_cdf_below (Binomial (0.5, 2)) (-.max_float));
  assert_equal 1.0 (cdistr_cdf_below (Binomial (0.5, 2)) max_float)

let test_inferred_coincident_cut_atomic_masses _test_ctxt =
  let expr =
    Slice.Parse.parse_expr
      "let x = binomial(0.5, 2) in x <= 1 && x >= 1"
  in
  let discretized =
    expr |> Slice.Inference.infer |> Slice.Discretization.discretize_top
  in
  match discretized with
  | Slice.Ast.ExprNode
      (Slice.Ast.Let (_, Slice.Ast.ExprNode (Slice.Ast.DistrCase cases), _)) ->
      assert_float_list [0.25; 0.5; 0.25] (List.map snd cases)
  | _ -> assert_failure "expected the inferred Binomial sample to discretize"

let test_atomic_interval_masses _test_ctxt =
  let open Slice in
  let poisson = Distributions.Poisson 1.0 in
  let p0 = exp (-1.0) in
  assert_float_list [0.0; p0; 1.0 -. p0]
    (Discretization.prob_cdistr_intervals [Lats.Less 0.0; Lats.LessEq 0.0] poisson);
  assert_float_list [p0; 1.0 -. p0]
    (Discretization.prob_cdistr_intervals [Lats.Less 1.0] poisson);
  let bernoulli = Distributions.Binomial (0.5, 1) in
  assert_float_list [0.5; 0.5]
    (Discretization.prob_cdistr_intervals [Lats.Less 1.0] bernoulli);
  assert_float_list [0.0; 0.5; 0.5]
    (Discretization.prob_cdistr_intervals [Lats.Less 0.0; Lats.LessEq 0.0] bernoulli)

let test_atomic_cdf_between_integers _test_ctxt =
  let open Slice.Distributions in
  let poisson = Poisson 1.0 in
  assert_equal 0.0 (cdistr_cdf poisson (-0.5));
  assert_equal 0.0 (cdistr_cdf_left poisson 0.0);
  assert_bool "right-continuous Poisson CDF includes the atom at zero"
    (abs_float (cdistr_cdf poisson 0.0 -. exp (-1.0)) < 1e-12);
  assert_equal 1.0 (cdistr_cdf (Poisson 0.0) 0.0);
  assert_equal 0.0 (cdistr_cdf_left (Poisson 0.0) 0.0);
  assert_equal 1.0 (cdistr_cdf (Binomial (0.0, 0)) 0.0);
  assert_equal 0.0 (cdistr_cdf_left (Binomial (0.0, 0)) 0.0)

let test_degenerate_uniform_cuts _test_ctxt =
  let open Slice in
  let point_mass = Distributions.Uniform (1.0, 1.0) in
  assert_float_list [0.0; 1.0]
    (Discretization.prob_cdistr_intervals [Lats.Less 1.0] point_mass);
  assert_float_list [1.0; 0.0]
    (Discretization.prob_cdistr_intervals [Lats.LessEq 1.0] point_mass);
  assert_float_list [0.0; 1.0; 0.0]
    (Discretization.prob_cdistr_intervals
       [Lats.Less 1.0; Lats.LessEq 1.0] point_mass)

let test_integer_cdf_large_boundary _test_ctxt =
  let open Slice.Distributions in
  let uint_max = 4294967295 in
  let uint_max_float = float_of_int uint_max in
  let seen = ref None in
  ignore
    (integer_cdf ~strict:false ~upper_support:None
       (fun k -> seen := Some k; 0.0) uint_max_float);
  assert_equal (Some uint_max) !seen;
  ignore
    (integer_cdf ~strict:true ~upper_support:None
       (fun k -> seen := Some k; 0.0) (uint_max_float +. 1.0));
  assert_equal (Some uint_max) !seen;
  let range_error =
    Invalid_argument "integer CDF boundary is outside the GSL unsigned-int range"
  in
  assert_raises range_error
    (fun () -> ignore (cdistr_cdf (Poisson 1.0) (uint_max_float +. 1.0)));
  assert_raises range_error
    (fun () -> ignore (cdistr_cdf_left (Poisson 1.0) (uint_max_float +. 2.0)))

let test_atomic_parameter_domains _test_ctxt =
  let open Slice in
  assert_equal (Ok (Distributions.Poisson 0.0))
    (Distributions.get_cdistr_from_single_arg_kind Ast.DPoisson 0.0);
  assert_equal (Ok (Distributions.Binomial (0.0, 0)))
    (Distributions.get_cdistr_from_two_arg_kind Ast.DBinomial 0.0 0.0);
  assert_bool "binomial rejects p > 1"
    (Result.is_error (Distributions.get_cdistr_from_two_arg_kind Ast.DBinomial 1.1 2.0));
  assert_bool "binomial rejects fractional n"
    (Result.is_error (Distributions.get_cdistr_from_two_arg_kind Ast.DBinomial 0.5 2.5));
  assert_equal (Ok (Distributions.Binomial (0.5, 4294967295)))
    (Distributions.get_cdistr_from_two_arg_kind Ast.DBinomial 0.5 4294967295.0);
  assert_bool "binomial rejects n above the GSL unsigned-int range"
    (Result.is_error
       (Distributions.get_cdistr_from_two_arg_kind Ast.DBinomial 0.5 4294967296.0));
  assert_raises (Invalid_argument "Binomial n is outside the GSL unsigned-int range")
    (fun () ->
      ignore (Distributions.cdistr_cdf (Distributions.Binomial (0.5, -1)) 0.0))
(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "canonical coincident-cut order" >:: test_canonical_coincident_cut_order;
     "coincident-cut indices" >:: test_coincident_cut_indices;
     "coincident-cut atomic masses" >:: test_coincident_cut_atomic_masses;
     "coincident-cut continuous masses" >:: test_coincident_cut_continuous_masses;
     "coincident-cut degenerate uniform masses" >::
       test_coincident_cut_degenerate_uniform_masses;
     "integer CDF threshold safety" >:: test_integer_cdf_threshold_safety;
     "inferred coincident-cut atomic masses" >::
       test_inferred_coincident_cut_atomic_masses;
     "test_atomic_interval_masses" >:: test_atomic_interval_masses;
     "test_atomic_cdf_between_integers" >:: test_atomic_cdf_between_integers;
     "test_degenerate_uniform_cuts" >:: test_degenerate_uniform_cuts;
     "test_integer_cdf_large_boundary" >:: test_integer_cdf_large_boundary;
     "test_atomic_parameter_domains" >:: test_atomic_parameter_domains;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
