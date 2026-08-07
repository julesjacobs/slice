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
  assert_raises expected (fun () -> ignore (Slice.Interp.run expr))

let test_singleton_distribution_parameter_is_evaluated _test_ctxt =
  let open Slice in
  let param_cuts = Lats.CutLat.create (Lats.Finite Lats.CutSet.empty) in
  let param_values =
    Lats.FloatLat.create (Lats.Finite (Lats.FloatSet.singleton 1.0))
  in
  let param_ty = Ast.TFloat (param_cuts, param_values) in
  let erroring_param =
    (param_ty, Ast.TAExprNode (Ast.RuntimeError "singleton parameter failed"))
  in
  let result_cuts =
    Lats.CutLat.create
      (Lats.Finite (Lats.CutSet.singleton (Lats.Less 0.5)))
  in
  let result_values = Lats.FloatLat.create Lats.Top in
  let result_ty = Ast.TFloat (result_cuts, result_values) in
  let typed_sample =
    ( result_ty,
      Ast.TAExprNode
        (Ast.Sample (Ast.Distr1 (Ast.DExponential, erroring_param))) )
  in
  let discretized = Discretization.discretize typed_sample in
  assert_raises (Interp.RuntimeError "singleton parameter failed")
    (fun () -> ignore (Interp.run discretized))

let singleton_float_type value =
  let open Slice in
  Ast.TFloat
    ( Lats.CutLat.create (Lats.Finite Lats.CutSet.empty),
      Lats.FloatLat.create
        (Lats.Finite (Lats.FloatSet.singleton value)) )

let finite_sample_type () =
  let open Slice in
  Ast.TFloat
    ( Lats.CutLat.create
        (Lats.Finite (Lats.CutSet.singleton (Lats.Less 0.5))),
      Lats.FloatLat.create Lats.Top )

let two_parameter_sample first second =
  let open Slice in
  ( finite_sample_type (),
    Ast.TAExprNode
      (Ast.Sample (Ast.Distr2 (Ast.DUniform, first, second))) )

let test_two_parameters_evaluate_left_to_right _test_ctxt =
  let open Slice in
  let first =
    (singleton_float_type 0.0, Ast.TAExprNode (Ast.RuntimeError "first parameter"))
  in
  let second =
    (singleton_float_type 1.0, Ast.TAExprNode (Ast.RuntimeError "second parameter"))
  in
  let discretized = Discretization.discretize (two_parameter_sample first second) in
  assert_raises (Interp.RuntimeError "first parameter")
    (fun () -> ignore (Interp.run discretized))

let test_first_parameter_observation_precedes_second_error _test_ctxt =
  let open Slice in
  let typed_false = (Ast.TBool, Ast.TAExprNode (Ast.BoolConst false)) in
  let typed_observe =
    (Ast.TUnit, Ast.TAExprNode (Ast.Observe typed_false))
  in
  let typed_zero =
    (singleton_float_type 0.0, Ast.TAExprNode (Ast.Const 0.0))
  in
  let first =
    ( singleton_float_type 0.0,
      Ast.TAExprNode (Ast.Seq (typed_observe, typed_zero)) )
  in
  let second =
    (singleton_float_type 1.0, Ast.TAExprNode (Ast.RuntimeError "second parameter"))
  in
  let discretized = Discretization.discretize (two_parameter_sample first second) in
  assert_raises Interp.ObserveFailure
    (fun () -> ignore (Interp.run discretized))

let test_empty_dispatch_domain_is_an_invariant_failure _test_ctxt =
  let open Slice in
  let empty_param_type =
    Ast.TFloat
      ( Lats.CutLat.create (Lats.Finite Lats.CutSet.empty),
        Lats.FloatLat.create (Lats.Finite Lats.FloatSet.empty) )
  in
  let empty_param =
    (empty_param_type, Ast.TAExprNode (Ast.RuntimeError "must not be emitted"))
  in
  match Discretization.discretize
          (finite_sample_type (),
           Ast.TAExprNode (Ast.Sample (Ast.Distr1 (Ast.DExponential, empty_param))))
  with
  | _ -> assert_failure "Expected an empty finite dispatch domain to fail"
  | exception Failure message ->
      assert_equal
        "Internal error: finite sample parameter lambda has no nonempty finite dispatch domain"
        message

let rec contains_recursive_divergence (Slice.Ast.ExprNode node) =
  let open Slice.Ast in
  match node with
  | FuncApp
      ( ExprNode (Fix (f, x, ExprNode (FuncApp (ExprNode (Var f'), ExprNode (Var x'))))),
        ExprNode Unit ) ->
      f = f' && x = x'
  | Let (_, first, second) | And (first, second) | Or (first, second)
  | Pair (first, second) | FuncApp (first, second) | Assign (first, second)
  | Seq (first, second) | FinEq (first, second, _)
  | Cmp (_, first, second, _) | FinCmp (_, first, second, _, _)
  | Cons (first, second) ->
      contains_recursive_divergence first || contains_recursive_divergence second
  | LoopApp (first, second, _) ->
      contains_recursive_divergence first || contains_recursive_divergence second
  | If (condition, then_branch, else_branch) ->
      contains_recursive_divergence condition
      || contains_recursive_divergence then_branch
      || contains_recursive_divergence else_branch
  | DistrCase cases ->
      List.exists (fun (case, _) -> contains_recursive_divergence case) cases
  | Sample (Distr1 (_, parameter)) -> contains_recursive_divergence parameter
  | Sample (Distr2 (_, first, second)) ->
      contains_recursive_divergence first || contains_recursive_divergence second
  | First child | Second child | Fun (_, child) | Not child | Observe child
  | Ref child | Deref child | Fix (_, _, child) -> contains_recursive_divergence child
  | MatchList (value, nil_case, _, _, cons_case) ->
      contains_recursive_divergence value
      || contains_recursive_divergence nil_case
      || contains_recursive_divergence cons_case
  | Var _ | Const _ | BoolConst _ | FinConst _ | Nil | Unit
  | RuntimeError _ -> false

let test_final_dispatch_arm_fails_closed _test_ctxt =
  let open Slice in
  let parameter =
    (singleton_float_type 1.0, Ast.TAExprNode (Ast.Const 1.0))
  in
  let transformed =
    Discretization.discretize
      (finite_sample_type (),
       Ast.TAExprNode (Ast.Sample (Ast.Distr1 (Ast.DExponential, parameter))))
  in
  assert_bool "final dispatch arm must fail closed"
    (contains_recursive_divergence transformed)

let test_unmatched_dispatch_index_uses_typed_divergence _test_ctxt =
  let open Slice in
  let parameter_type =
    Ast.TFloat
      ( Lats.CutLat.create
          (Lats.Finite (Lats.CutSet.singleton (Lats.Less 0.5))),
        Lats.FloatLat.create
          (Lats.Finite (Lats.FloatSet.singleton 1.0)) )
  in
  let mismatched_parameter =
    (parameter_type, Ast.TAExprNode (Ast.Const 0.0))
  in
  let transformed =
    Discretization.discretize
      ( finite_sample_type (),
        Ast.TAExprNode
          (Ast.Sample (Ast.Distr1 (Ast.DExponential, mismatched_parameter))) )
  in
  assert_bool "mismatched final dispatch must contain typed divergence"
    (contains_recursive_divergence transformed);
  let inferred_type, _ = Inference.infer transformed in
  match Ast.force inferred_type with
  | Ast.TFin n -> assert_equal 2 n
  | _ -> assert_failure "divergent fallback did not retain the dispatch result type"

let valid_dynamic_uniform () =
  let open Slice in
  let dynamic_type =
    Ast.TFloat
      ( Lats.CutLat.create
          (Lats.Finite (Lats.CutSet.singleton (Lats.Less 0.5))),
        Lats.FloatLat.create
          (Lats.Finite (Lats.FloatSet.of_list [0.0; 1.0])) )
  in
  let typed_true = (Ast.TBool, Ast.TAExprNode (Ast.BoolConst true)) in
  let typed_zero = (dynamic_type, Ast.TAExprNode (Ast.Const 0.0)) in
  let typed_one = (dynamic_type, Ast.TAExprNode (Ast.Const 1.0)) in
  let first =
    (dynamic_type, Ast.TAExprNode (Ast.If (typed_true, typed_zero, typed_one)))
  in
  let second_type = singleton_float_type 2.0 in
  let second = (second_type, Ast.TAExprNode (Ast.Const 2.0)) in
  Discretization.discretize (two_parameter_sample first second)

let contains_substring haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec search offset =
    offset + needle_length <= haystack_length
    && (String.sub haystack offset needle_length = needle || search (offset + 1))
  in
  needle_length = 0 || search 0

let test_dynamic_sampler_exports_to_all_backends _test_ctxt =
  Slice.Util.var_counter := 0;
  let transformed = valid_dynamic_uniform () in
  let dice = Slice.To_dice.string_of_expr transformed in
  let roulette = Slice.To_roulette.string_of_expr transformed in
  assert_bool "Dice must hoist generated recursive divergence"
    (contains_substring dice "fun _slice_diverge");
  assert_bool "Dice must not emit unsupported fix-expression syntax"
    (not (contains_substring dice "fix "));
  assert_bool "Roulette must bind generated recursive divergence as an expression"
    (contains_substring roulette "(letrec ([_slice_diverge");
  assert_bool "Roulette must not embed a define inside an application"
    (not (contains_substring roulette "((define"));
  ignore (Slice.To_mc.to_mc transformed)

let test_generated_binders_are_hygienic _test_ctxt =
  let open Slice in
  Util.var_counter := 0;
  let first_type =
    Ast.TFloat
      ( Lats.CutLat.create
          (Lats.Finite (Lats.CutSet.singleton (Lats.Less 0.5))),
        Lats.FloatLat.create
          (Lats.Finite (Lats.FloatSet.of_list [0.0; 1.0])) )
  in
  let typed_bool = (Ast.TBool, Ast.TAExprNode (Ast.BoolConst true)) in
  let typed_zero = (first_type, Ast.TAExprNode (Ast.Const 0.0)) in
  let typed_one = (first_type, Ast.TAExprNode (Ast.Const 1.0)) in
  let first =
    (first_type, Ast.TAExprNode (Ast.If (typed_bool, typed_zero, typed_one)))
  in
  let second_type = singleton_float_type 2.0 in
  let second =
    (second_type, Ast.TAExprNode (Ast.Var "_disc_a1"))
  in
  let typed_two = (second_type, Ast.TAExprNode (Ast.Const 2.0)) in
  let sample = two_parameter_sample first second in
  let source =
    (fst sample, Ast.TAExprNode (Ast.Let ("_disc_a1", typed_two, sample)))
  in
  let transformed = Discretization.discretize source in
  let rec generated_binder_avoids_source (Ast.ExprNode node) =
    match node with
    | Ast.Let ("_disc_a1", _, body) -> generated_binder_avoids_source body
    | Ast.Let (name, _, _) when String.length name >= 7
                                && String.sub name 0 7 = "_disc_a" ->
        name <> "_disc_a1"
    | Ast.Let (_, first, second) ->
        generated_binder_avoids_source first || generated_binder_avoids_source second
    | Ast.If (first, second, third) ->
        generated_binder_avoids_source first
        || generated_binder_avoids_source second
        || generated_binder_avoids_source third
    | _ -> false
  in
  assert_bool "generated binder captured a source identifier"
    (generated_binder_avoids_source transformed)

let test_backends_reject_runtime_error _test_ctxt =
  let error = Slice.Ast.ExprNode (Slice.Ast.RuntimeError "unsupported") in
  assert_raises
    (Failure "Dice backend cannot serialize RuntimeError: unsupported")
    (fun () -> ignore (Slice.To_dice.string_of_expr error));
  assert_raises
    (Failure "Roulette backend cannot serialize RuntimeError: unsupported")
    (fun () -> ignore (Slice.To_roulette.string_of_expr error))

(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "uniform_requires_strict_bounds" >:: test_uniform_requires_strict_bounds;
     "uniform_cdf_is_clamped" >:: test_uniform_cdf_is_clamped;
     "uniform_cdf_handles_extreme_finite_bounds" >:: test_uniform_cdf_handles_extreme_finite_bounds;
     "uniform_cdf_rejects_nan_query" >:: test_uniform_cdf_rejects_nan_query;
     "uniform_requires_finite_bounds" >:: test_uniform_requires_finite_bounds;
     "degenerate_uniform_operations_fail" >:: test_degenerate_uniform_operations_fail;
     "degenerate_uniform_program_fails" >:: test_degenerate_uniform_program_fails;
     "test_singleton_distribution_parameter_is_evaluated"
       >:: test_singleton_distribution_parameter_is_evaluated;
     "test_two_parameters_evaluate_left_to_right"
       >:: test_two_parameters_evaluate_left_to_right;
     "test_first_parameter_observation_precedes_second_error"
       >:: test_first_parameter_observation_precedes_second_error;
     "test_empty_dispatch_domain_is_an_invariant_failure"
       >:: test_empty_dispatch_domain_is_an_invariant_failure;
     "test_final_dispatch_arm_fails_closed" >:: test_final_dispatch_arm_fails_closed;
     "test_unmatched_dispatch_index_uses_typed_divergence"
       >:: test_unmatched_dispatch_index_uses_typed_divergence;
     "test_dynamic_sampler_exports_to_all_backends"
       >:: test_dynamic_sampler_exports_to_all_backends;
     "test_generated_binders_are_hygienic" >:: test_generated_binders_are_hygienic;
     "test_backends_reject_runtime_error" >:: test_backends_reject_runtime_error;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
