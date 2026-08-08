open OUnit2
open Slice.Ast
open Slice.Lats

let assert_cut_top message bag =
  match CutLat.get bag with
  | Top -> ()
  | Finite _ -> assert_failure message

let cut_bag_of_type = function
  | TFloat (bag, _) -> bag
  | ty -> assert_failure ("expected float type, got " ^ Slice.Pretty.string_of_ty ty)

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
  let probs =
    Slice.Discretization.interval_probabilities coincident_cuts
      (Slice.Distributions.Binomial (0.5, 2))
  in
  assert_float_list [0.25; 0.5; 0.25] probs

let test_coincident_cut_continuous_masses _test_ctxt =
  let probs =
    Slice.Discretization.interval_probabilities coincident_cuts
      (Slice.Distributions.Uniform (0.0, 2.0))
  in
  assert_float_list [0.5; 0.0; 0.5] probs

let test_coincident_cut_degenerate_uniform_masses _test_ctxt =
  assert_raises (Invalid_argument "Uniform low must be less than high")
    (fun () ->
      ignore
        (Slice.Discretization.interval_probabilities coincident_cuts
           (Slice.Distributions.Uniform (1.0, 1.0))))

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

let test_nonfinite_cuts_are_rejected _test_ctxt =
  let open Slice in
  List.iter
    (fun cut ->
      assert_raises (Invalid_argument "Cut boundaries must be finite")
        (fun () ->
          ignore
            (Discretization.interval_probabilities (Lats.CutSet.singleton cut)
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
  assert_bool "Roulette must serialize ordinary bindings as local lets"
    (contains_substring roulette "(let ([_disc_");
  assert_bool "Roulette must not nest a generated define in a definition RHS"
    (not (contains_substring roulette "(define x (define"));
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

let test_same_topness_propagates_both_directions _test_ctxt =
  let check message set_before_registration select_source =
    let b1 = fresh_cut_bag () in
    let b2 = fresh_cut_bag () in
    let source, target = select_source b1 b2 in
    if set_before_registration then CutLat.leq (CutLat.create Top) source;
    Slice.Inference.enforce_same_topness b1 b2;
    if not set_before_registration then CutLat.leq (CutLat.create Top) source;
    assert_cut_top message target
  in
  check "pre-existing topness did not propagate from the first cut bag"
    true (fun b1 b2 -> b1, b2);
  check "delayed topness did not propagate from the first cut bag"
    false (fun b1 b2 -> b1, b2);
  check "pre-existing topness did not propagate from the second cut bag"
    true (fun b1 b2 -> b2, b1);
  check "delayed topness did not propagate from the second cut bag"
    false (fun b1 b2 -> b2, b1)

let test_recovers_top_requires_top_cuts _test_ctxt =
  let cuts = fresh_cut_bag () in
  let values = fresh_float_bag () in
  Slice.Inference.enforce_recovers cuts values;
  FloatLat.leq (FloatLat.create Top) values;
  assert_cut_top "recovers(B, top) left B finite" cuts

let test_continuous_comparison_keeps_sampler_parameters_float _test_ctxt =
  let expr = Slice.Parse.parse_expr "uniform(0, 1) < uniform(0, 1)" in
  let typed = Slice.Inference.infer expr in
  let assert_sampler_and_parameters_top = function
    | TFloat (sample_cuts, _),
      TAExprNode (Sample (Distr2 (_, (arg1_ty, _), (arg2_ty, _)))) ->
        assert_cut_top "continuous comparison left a sampler cut set finite" sample_cuts;
        assert_cut_top "continuous comparison discretized the first parameter" (cut_bag_of_type arg1_ty);
        assert_cut_top "continuous comparison discretized the second parameter" (cut_bag_of_type arg2_ty)
    | _ -> assert_failure "expected a two-argument sampler"
  in
  (match typed with
   | TBool, TAExprNode (Cmp (_, left, right, _)) ->
       assert_sampler_and_parameters_top left;
       assert_sampler_and_parameters_top right
   | _ -> assert_failure "expected a boolean comparison");
  match Slice.Discretization.discretize typed with
  | ExprNode
      (Cmp
        (_, ExprNode (Sample (Distr2 (_, ExprNode (Const _), ExprNode (Const _)))),
         ExprNode (Sample (Distr2 (_, ExprNode (Const _), ExprNode (Const _)))), _)) ->
      ()
  | _ -> assert_failure "continuous sampler parameters were not preserved as floats"

let test_finite_comparison_uses_right_operand_cuts _test_ctxt =
  let check expression expected_cut =
    match Slice.Inference.infer (Slice.Parse.parse_expr expression) with
    | TBool, TAExprNode (Cmp (_, (TFloat (cuts, _), _), _, _)) ->
        (match CutLat.get cuts with
         | Top -> assert_failure "finite comparison unexpectedly forced top cuts"
         | Finite actual ->
             assert_bool "finite comparison added redundant left-operand cuts"
               (CutSet.equal (CutSet.singleton expected_cut) actual))
    | _ -> assert_failure "expected a float comparison"
  in
  check "1 < 2" (Less 2.0);
  check "1 <= 2" (LessEq 2.0)

let test_sampled_distribution_parameter_forces_top _test_ctxt =
  let expr = Slice.Parse.parse_expr "exponential(uniform(0, 1)) < 0" in
  let typed = Slice.Inference.infer expr in
  match typed with
  | TBool,
    TAExprNode
      (Cmp
        (_, (TFloat (outer_cuts, _),
             TAExprNode (Sample (Distr1 (_, (TFloat (inner_cuts, _), _))))),
         _, _)) ->
      assert_cut_top "sampled parameter did not force its own cut set to top" inner_cuts;
      assert_cut_top "sampled parameter topness did not propagate to sampler output" outer_cuts
  | _ -> assert_failure "expected a nested one-argument sampler comparison"

let test_unit_reflexivity_in_if _test_ctxt =
  let expr = Slice.Parse.parse_expr "if true then () else ()" in
  match Slice.Inference.infer expr with
  | ty, _ ->
      (match Slice.Ast.force ty with
       | TUnit -> ()
       | _ -> assert_failure "unit branches did not infer unit")

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
     "same_topness_propagates_both_directions" >:: test_same_topness_propagates_both_directions;
     "recovers_top_requires_top_cuts" >:: test_recovers_top_requires_top_cuts;
     "continuous_comparison_keeps_sampler_parameters_float" >:: test_continuous_comparison_keeps_sampler_parameters_float;
     "finite_comparison_uses_right_operand_cuts" >:: test_finite_comparison_uses_right_operand_cuts;
     "sampled_distribution_parameter_forces_top" >:: test_sampled_distribution_parameter_forces_top;
     "unit_reflexivity_in_if" >:: test_unit_reflexivity_in_if;
     ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
