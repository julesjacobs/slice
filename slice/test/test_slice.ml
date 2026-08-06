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

let test_same_topness_propagates_both_directions _test_ctxt =
  let b1 = fresh_cut_bag () in
  let b2 = fresh_cut_bag () in
  Slice.Inference.enforce_same_topness b1 b2;
  CutLat.leq (CutLat.create Top) b2;
  assert_cut_top "topness did not propagate from the second cut bag" b1

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
    ["test_typing" >:: test_typing;
     "same_topness_propagates_both_directions" >:: test_same_topness_propagates_both_directions;
     "recovers_top_requires_top_cuts" >:: test_recovers_top_requires_top_cuts;
     "continuous_comparison_keeps_sampler_parameters_float" >:: test_continuous_comparison_keeps_sampler_parameters_float;
     "sampled_distribution_parameter_forces_top" >:: test_sampled_distribution_parameter_forces_top;
     "unit_reflexivity_in_if" >:: test_unit_reflexivity_in_if;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
