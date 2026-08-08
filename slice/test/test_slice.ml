open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Slice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Slice.Inference.infer expr in
    ignore (Slice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

let assert_discretization_fails_with expected source =
  let expr = Slice.Parse.parse_expr source in
  match Slice.Discretization.discretize_top (Slice.Inference.infer expr) with
  | _ -> assert_failure ("Expected discretization to reject: " ^ source)
  | exception Failure message ->
      assert_bool
        (Printf.sprintf "Expected error containing %S, got %S" expected message)
        (String.contains message expected.[0]
         && let expected_len = String.length expected in
            let message_len = String.length message in
            let rec search offset =
              offset + expected_len <= message_len
              && (String.sub message offset expected_len = expected || search (offset + 1))
            in
            search 0)

let test_empty_distribution_parameter_rejected _test_ctxt =
  assert_discretization_fails_with "nonempty parameter value set"
    "let unused = fun x -> uniform(x, 1) in true"

let test_recursive_distribution_parameter_rejected _test_ctxt =
  assert_discretization_fails_with "nonempty parameter value set"
    "let diverging = fix f x := f x in uniform(diverging 0, 1) < 0.5"

let test_raw_discretize_checks_distribution_parameters _test_ctxt =
  let expr = Slice.Parse.parse_expr
      "let unused = fun x -> uniform(x, 1) in true"
  in
  match Slice.Discretization.discretize (Slice.Inference.infer expr) with
  | _ -> assert_failure "Expected raw discretize to validate finite dispatch domains"
  | exception Failure message ->
      assert_bool "Expected dispatchability failure"
        (let needle = "nonempty parameter value set" in
         let needle_len = String.length needle in
         let message_len = String.length message in
         let rec contains offset =
           offset + needle_len <= message_len
           && (String.sub message offset needle_len = needle || contains (offset + 1))
         in
         contains 0)

let remove_if_present path =
  if Sys.file_exists path then Sys.remove path

let compile_api_probe member =
  let source = Filename.temp_file "slice-discretization-api" ".ml" in
  let stem = Filename.chop_extension source in
  let channel = open_out source in
  output_string channel
    (Printf.sprintf "let _ = Slice.Discretization.%s\n" member);
  close_out channel;
  let include_dir =
    Filename.concat (Sys.getcwd ()) "../lib/.slice.objs/byte"
  in
  let command =
    Printf.sprintf "ocamlc -I %s -c %s >/dev/null 2>&1"
      (Filename.quote include_dir) (Filename.quote source)
  in
  let status = Sys.command command in
  List.iter remove_if_present [source; stem ^ ".cmi"; stem ^ ".cmo"];
  status

let test_unchecked_discretizer_is_not_public _test_ctxt =
  assert_equal 0 (compile_api_probe "discretize");
  assert_bool "The unchecked transformation helper must not be public"
    (compile_api_probe "discretize_unchecked" <> 0)

let rec contains_sample (Slice.Ast.ExprNode node) =
  let open Slice.Ast in
  match node with
  | Sample _ -> true
  | Let (_, e1, e2) | And (e1, e2) | Or (e1, e2) | Pair (e1, e2)
  | FuncApp (e1, e2) | Assign (e1, e2) | Seq (e1, e2)
  | Cmp (_, e1, e2, _) | FinCmp (_, e1, e2, _, _) | FinEq (e1, e2, _) ->
      contains_sample e1 || contains_sample e2
  | If (e1, e2, e3) ->
      contains_sample e1 || contains_sample e2 || contains_sample e3
  | DistrCase cases -> List.exists (fun (e, _) -> contains_sample e) cases
  | First e | Second e | Fun (_, e) | Not e | Observe e | Ref e | Deref e ->
      contains_sample e
  | LoopApp (e1, e2, _) -> contains_sample e1 || contains_sample e2
  | Fix (_, _, e) -> contains_sample e
  | Cons (head, tail) -> contains_sample head || contains_sample tail
  | MatchList (value, nil_case, _, _, cons_case) ->
      contains_sample value || contains_sample nil_case || contains_sample cons_case
  | Var _ | Const _ | BoolConst _ | FinConst _ | Nil | Unit | RuntimeError _ -> false

let test_nonempty_parameter_emits_no_sample_fallback _test_ctxt =
  let expr = Slice.Parse.parse_expr
      "let p = if true then 1 else 2 in uniform(0, p) < 0.5"
  in
  let discretized = Slice.Discretization.discretize_top (Slice.Inference.infer expr) in
  assert_bool "A dispatchable parameter must not leave a sampler fallback" (not (contains_sample discretized))

let test_top_level_function_propagates_top_before_validation _test_ctxt =
  let expr = Slice.Parse.parse_expr "fun x -> uniform(x, 1)" in
  let discretized = Slice.Discretization.discretize_top (Slice.Inference.infer expr) in
  assert_bool
    "A continuous sample in the top-level return interface must remain continuous"
    (contains_sample discretized)

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

let test_discrete_rejects_unrepresentable_prefix _test_ctxt =
  List.iter
    (fun weights ->
      assert_raises
        (Failure
           "Discrete distribution contains a positive weight that cannot be represented cumulatively")
        (fun () -> ignore (Slice.Ast.normalize_probability_weights weights)))
    [[1.0; 1e-20; 0.0]; [1.0; 1e-20]]

let test_generated_tolerance_is_capped _test_ctxt =
  assert_equal 1e-6 (Slice.Ast.probability_sum_tolerance Slice.Ast.Generated 100_000)

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

let var x = Slice.Ast.ExprNode (Slice.Ast.Var x)

let test_fix_substitution_freshens_binders _test_ctxt =
  let open Slice.Ast in
  let body = ExprNode (Pair (var "z", ExprNode (Pair (var "f", var "y")))) in
  let expr = ExprNode (Fix ("f", "y", body)) in
  let value = ExprNode (Pair (var "f", var "y")) in
  match Slice.To_mc.subst "z" value expr with
  | ExprNode (Fix (f, y, _)) ->
      assert_bool "recursive binder should be freshened" (f <> "f");
      assert_bool "argument binder should be freshened" (y <> "y");
      let free = Slice.To_mc.free_vars (Slice.To_mc.subst "z" value expr) in
      assert_bool "substituted f should remain free" (Slice.To_mc.StringSet.mem "f" free);
      assert_bool "substituted y should remain free" (Slice.To_mc.StringSet.mem "y" free)
  | _ -> assert_failure "expected a fix expression"

let test_match_substitution_freshens_binders _test_ctxt =
  let open Slice.Ast in
  let cons_br = ExprNode (Pair (var "z", ExprNode (Pair (var "h", var "t")))) in
  let expr = ExprNode (MatchList (var "xs", ExprNode Nil, "h", "t", cons_br)) in
  let value = ExprNode (Pair (var "h", var "t")) in
  match Slice.To_mc.subst "z" value expr with
  | ExprNode (MatchList (_, _, h, t, _)) ->
      assert_bool "head binder should be freshened" (h <> "h");
      assert_bool "tail binder should be freshened" (t <> "t");
      let free = Slice.To_mc.free_vars (Slice.To_mc.subst "z" value expr) in
      assert_bool "substituted h should remain free" (Slice.To_mc.StringSet.mem "h" free);
      assert_bool "substituted t should remain free" (Slice.To_mc.StringSet.mem "t" free)
  | _ -> assert_failure "expected a list match"

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

let test_uniform_cdf_rejects_nan_query _test_ctxt =
  let dist = Slice.Distributions.Uniform (0.0, 1.0) in
  assert_raises (Invalid_argument "CDF is undefined at NaN")
    (fun () -> ignore (Slice.Distributions.cdistr_cdf dist nan))

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
     "test_empty_distribution_parameter_rejected" >:: test_empty_distribution_parameter_rejected;
     "test_recursive_distribution_parameter_rejected" >:: test_recursive_distribution_parameter_rejected;
     "test_raw_discretize_checks_distribution_parameters"
       >:: test_raw_discretize_checks_distribution_parameters;
     "test_unchecked_discretizer_is_not_public"
       >:: test_unchecked_discretizer_is_not_public;
     "test_nonempty_parameter_emits_no_sample_fallback" >:: test_nonempty_parameter_emits_no_sample_fallback;
     "test_top_level_function_propagates_top_before_validation"
       >:: test_top_level_function_propagates_top_before_validation;
      "test_discrete_weights" >:: test_discrete_weights;
     "test_discrete_rejects_nonfinite" >:: test_discrete_rejects_nonfinite;
     "test_discrete_rejects_unrepresentable_prefix" >::
       test_discrete_rejects_unrepresentable_prefix;
     "test_generated_tolerance_is_capped" >:: test_generated_tolerance_is_capped;
      "test_discrete_final_boundary" >:: test_discrete_final_boundary;
      "test_probability_export_precision" >:: test_probability_export_precision;
     "test_fix_substitution_freshens_binders" >:: test_fix_substitution_freshens_binders;
     "test_match_substitution_freshens_binders" >:: test_match_substitution_freshens_binders;
     "uniform_requires_strict_bounds" >:: test_uniform_requires_strict_bounds;
     "uniform_cdf_is_clamped" >:: test_uniform_cdf_is_clamped;
     "uniform_cdf_handles_extreme_finite_bounds" >:: test_uniform_cdf_handles_extreme_finite_bounds;
     "uniform_cdf_rejects_nan_query" >:: test_uniform_cdf_rejects_nan_query;
     "uniform_requires_finite_bounds" >:: test_uniform_requires_finite_bounds;
     "degenerate_uniform_operations_fail" >:: test_degenerate_uniform_operations_fail;
     "degenerate_uniform_program_fails" >:: test_degenerate_uniform_program_fails;
     ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
