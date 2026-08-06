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
  assert_raises (Invalid_argument "Uniform low must be less than high")
    (fun () ->
      ignore
        (Slice.Discretization.interval_probabilities cuts
           (Slice.Distributions.Uniform (1.0, 1.0))))

let test_integer_cdf_threshold_safety _test_ctxt =
  let open Slice.Distributions in
  let nan = 0.0 /. 0.0 in
  assert_raises (Invalid_argument "CDF is undefined at NaN")
    (fun () -> ignore (cdistr_cdf (Poisson 1.0) nan));
  assert_raises (Invalid_argument "CDF is undefined at NaN")
    (fun () -> ignore (cdistr_cdf_below (Binomial (0.5, 2)) nan));
  assert_equal 0.0 (cdistr_cdf (Poisson 1.0) (-.max_float));
  assert_equal 1.0 (cdistr_cdf (Poisson 1.0) max_float);
  assert_equal 0.0 (cdistr_cdf_below (Binomial (0.5, 2)) (-.max_float));
  assert_equal 1.0 (cdistr_cdf_below (Binomial (0.5, 2)) max_float)

let test_nonfinite_cuts_are_rejected _test_ctxt =
  let open Slice in
  List.iter
    (fun cut ->
      assert_raises (Invalid_argument "Cut boundaries must be finite")
        (fun () ->
          ignore
            (Discretization.interval_probabilities [cut]
               (Distributions.Uniform (0.0, 1.0)))))
    [Lats.Less infinity; Lats.LessEq neg_infinity; Lats.Less nan]

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

let assert_invalid_discrete source =
  match Slice.Parse.parse_expr source with
  | _ -> assert_failure ("Expected invalid discrete weights: " ^ source)
  | exception Failure _ -> ()

let test_discrete_weights _test_ctxt =
  List.iter assert_invalid_discrete
    [ "discrete()";
      "discrete(0.2, 0.2)";
      "discrete(2, -1)";
      "discrete(0 / 0, 1)" ];
  let expr = Slice.Parse.parse_expr "discrete(0.33333, 0.66666)" in
  match expr with
  | Slice.Ast.ExprNode (Slice.Ast.DistrCase cases) ->
      let sum = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 cases in
      assert_equal ~cmp:(fun x y -> abs_float (x -. y) < 1e-12) 1.0 sum
  | _ -> assert_failure "Expected a discrete distribution"

let seven_way_source =
  "discrete(0.14285714285714285, 0.14285714285714285, \
   0.14285714285714285, 0.14285714285714285, 0.14285714285714285, \
   0.14285714285714285, 0.14285714285714285)"

let test_discrete_rejects_nonfinite _test_ctxt =
  List.iter
    (fun weights ->
      assert_raises (Failure "Discrete distribution weights must be finite")
        (fun () -> ignore (Slice.Ast.normalize_probability_weights weights)))
    [[nan; 1.0]; [infinity; 0.0]; [neg_infinity; 1.0]]

let test_discrete_final_boundary _test_ctxt =
  let expr = Slice.Parse.parse_expr seven_way_source in
  match expr with
  | Slice.Ast.ExprNode (Slice.Ast.DistrCase cases) ->
      let selected = Slice.Interp.select_distr_case (1.0 -. epsilon_float) cases in
      assert_equal (Slice.Ast.ExprNode (Slice.Ast.FinConst (6, 7))) selected
  | _ -> assert_failure "Expected a discrete distribution"

let test_probability_export_precision _test_ctxt =
  let expr = Slice.Parse.parse_expr seven_way_source in
  let dice = Slice.To_dice.string_of_expr expr in
  assert_equal
    ~printer:(fun x -> x)
    "discrete(0.14285714285714288, 0.14285714285714288, 0.14285714285714288, 0.14285714285714288, 0.14285714285714288, 0.14285714285714288, 0.14285714285714268)"
    dice;
  let roulette = Slice.To_roulette.string_of_expr expr in
  assert_equal ~printer:(fun x -> x)
    "(if (flip 0.14285714285714288) 0 (if (flip 0.16666666666666671) 1 (if (flip 0.20000000000000007) 2 (if (flip 0.25000000000000011) 3 (if (flip 0.33333333333333354) 4 (if (flip 0.50000000000000044) 5 6))))))"
    roulette;
  let mc = Slice.To_mc.to_mc expr in
  let outgoing =
    List.filter (fun (t : Slice.To_mc.transition) -> t.src = 0) mc.transitions
  in
  let sum =
    List.fold_left
      (fun acc (t : Slice.To_mc.transition) -> acc +. t.prob)
      0.0 outgoing
  in
  assert_equal ~cmp:(fun x y -> x = y) 1.0 sum;
  let path = Filename.temp_file "slice-seven-way" ".tra" in
  Slice.To_mc.write_tra_file mc path;
  let channel = open_in path in
  let contents = really_input_string channel (in_channel_length channel) in
  close_in channel;
  Sys.remove path;
  assert_bool "Storm export preserves 17-digit probability"
    (let needle = "0.14285714285714288" in
     let n = String.length needle in
     let rec contains i =
       i + n <= String.length contents
       && (String.sub contents i n = needle || contains (i + 1))
     in contains 0)

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
     "canonical coincident-cut order" >:: test_canonical_coincident_cut_order;
     "coincident-cut indices" >:: test_coincident_cut_indices;
     "coincident-cut atomic masses" >:: test_coincident_cut_atomic_masses;
     "coincident-cut continuous masses" >:: test_coincident_cut_continuous_masses;
     "coincident-cut degenerate uniform masses" >::
       test_coincident_cut_degenerate_uniform_masses;
     "integer CDF threshold safety" >:: test_integer_cdf_threshold_safety;
     "nonfinite cuts are rejected" >:: test_nonfinite_cuts_are_rejected;
     "inferred coincident-cut atomic masses" >::
       test_inferred_coincident_cut_atomic_masses;
     "test_discrete_weights" >:: test_discrete_weights;
     "test_discrete_rejects_nonfinite" >:: test_discrete_rejects_nonfinite;
     "test_discrete_final_boundary" >:: test_discrete_final_boundary;
     "test_probability_export_precision" >:: test_probability_export_precision;
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
