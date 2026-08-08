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
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
