open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Slice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Slice.Inference.infer expr in
    ignore (Slice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

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

(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "test_singleton_distribution_parameter_is_evaluated"
       >:: test_singleton_distribution_parameter_is_evaluated;
     "test_two_parameters_evaluate_left_to_right"
       >:: test_two_parameters_evaluate_left_to_right;
     "test_first_parameter_observation_precedes_second_error"
       >:: test_first_parameter_observation_precedes_second_error;
     "test_empty_dispatch_domain_is_an_invariant_failure"
       >:: test_empty_dispatch_domain_is_an_invariant_failure;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
