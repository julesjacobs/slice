open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Slice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Slice.Inference.infer expr in
    ignore (Slice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

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

let test_finite_support_elaboration _test_ctxt =
  let expr =
    Slice.Parse.parse_expr
      "discrete(0.18: 0.05, 0.11: 0.24, 0.21: 0.37, 0.09: 0.58, 0.41: 0.89)"
  in
  let open Slice.Ast in
  (match expr with
   | ExprNode
       (Let
         ( choice,
           ExprNode (DistrCase index_cases),
           ExprNode (If (ExprNode (FinCmp (_, ExprNode (Var used), _, 5, _)), _, _)) )) ->
       assert_equal choice used;
       assert_equal 5 (List.length index_cases);
       List.iteri
         (fun i (case, _) -> assert_equal (ExprNode (FinConst (i, 5))) case)
         index_cases
   | _ ->
       assert_failure
         "weighted discrete syntax was not elaborated to a finite-index choice");
  ignore (Slice.Inference.infer expr)

let test_finite_support_elaboration_is_hygienic _test_ctxt =
  let expr =
    Slice.Parse.parse_expr
      "let __slice_discrete_choice_0 = 7 in discrete(0.5: __slice_discrete_choice_0, 0.5: 9)"
  in
  let open Slice.Ast in
  match expr with
  | ExprNode
      (Let
        ( "__slice_discrete_choice_0",
          _,
          ExprNode (Let (generated, _, _)) )) ->
      assert_equal "__slice_discrete_choice_1" generated
  | _ -> assert_failure "finite-support elaboration did not avoid variable capture"

let test_finite_support_elaboration_arities _test_ctxt =
  let open Slice.Ast in
  let check source expected_arity =
    match Slice.Parse.parse_expr source with
    | ExprNode (Let (_, ExprNode (DistrCase cases), _)) ->
        assert_equal expected_arity (List.length cases);
        let sum = List.fold_left (fun acc (_, p) -> acc +. p) 0.0 cases in
        assert_equal ~cmp:(fun x y -> abs_float (x -. y) < 1e-12) 1.0 sum
    | _ -> assert_failure "finite-support syntax was not elaborated"
  in
  check "discrete(1.0: 7)" 1;
  check "discrete(0.25: true, 0.75: false)" 2

let test_finite_support_elaboration_validates_probabilities _test_ctxt =
  List.iter assert_invalid_discrete
    [ "discrete(0 - 0.1: true, 1.1: false)";
      "discrete(0.2: true, 0.2: false)";
      "discrete()" ]
(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "test_discrete_weights" >:: test_discrete_weights;
     "test_discrete_rejects_nonfinite" >:: test_discrete_rejects_nonfinite;
     "test_discrete_final_boundary" >:: test_discrete_final_boundary;
     "test_probability_export_precision" >:: test_probability_export_precision;
     "test_finite_support_elaboration" >:: test_finite_support_elaboration;
     "test_finite_support_elaboration_is_hygienic" >:: test_finite_support_elaboration_is_hygienic;
     "test_finite_support_elaboration_arities" >:: test_finite_support_elaboration_arities;
     "test_finite_support_elaboration_validates_probabilities" >:: test_finite_support_elaboration_validates_probabilities;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
