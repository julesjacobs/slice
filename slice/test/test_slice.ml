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

(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "test_singleton_distribution_parameter_is_evaluated"
       >:: test_singleton_distribution_parameter_is_evaluated;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
