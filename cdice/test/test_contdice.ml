open OUnit2
open Contdice
open Contdice.Types (* To get access to Ast, TypeSystem, etc. modules *)
open Contdice.Types.Ast
open Contdice.Types.TypeSystem
open Contdice.Types.RuntimeValues

(* Helper function to assert that a specific exception is raised *)
let assert_raises_specific_exn (expected_exn_pattern : exn) (f : unit -> 'a) _test_ctxt =
  let raised_exception = ref None in
  try
    ignore (f ());
    () (* If no exception is raised, do nothing, assertion will fail later *)
  with
  | e -> raised_exception := Some e (* Store the raised exception *)
  in
  match !raised_exception with
  | Some actual_exn ->
      (match (expected_exn_pattern, actual_exn) with
       (* Comparing constructors for user-defined exceptions *)
       | (Lexer.LexError _, Lexer.LexError _) -> ()
       | (Parser.Error, Parser.Error) -> () (* Parser.Error is not a constructor with arguments for pattern matching like this *)
       | (Interp.TypeError _, Interp.TypeError _) -> ()
       | (Interp.UnboundVariableError _, Interp.UnboundVariableError _) -> ()
       | (Interp.InvalidArgumentsError _, Interp.InvalidArgumentsError _) -> ()
       | (Interp.ObserveFailure, Interp.ObserveFailure) -> ()
       (* Add other specific exception checks here if needed *)
       | _ ->
           if Printexc.to_string expected_exn_pattern = Printexc.to_string actual_exn then
             () (* Fallback for exceptions like Parser.Error that are not data constructors with specific patterns *)
           else
            assert_failure (Printf.sprintf "Expected exception %s but got %s"
                                (Printexc.to_string expected_exn_pattern)
                                (Printexc.to_string actual_exn)))
  | None ->
      assert_failure (Printf.sprintf "Expected exception %s but no exception was raised."
                        (Printexc.to_string expected_exn_pattern))

(* Helper to parse and evaluate a string, for interpreter tests *)
let eval_string s =
  let expr = Parse.parse_expr s in
  let texpr = elab expr in
  Interp.run (Contdice.discretize_top texpr) (* Assuming discretize_top is the way to get an expr for Interp.run *)

(* Test typechecking (existing test) *)
let test_typing _test_ctxt =
  let expr = Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = elab expr in
    ignore (Pretty.string_of_texpr texpr)
  with e ->
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

(* --- New Tests for Error Handling --- *)

(* Lexer Errors *)
let test_lexer_unterminated_comment =
  assert_raises_specific_exn (Lexer.LexError "") (fun () -> Parse.parse_expr "(* this is an unterminated comment")

(* Parser Errors *)
let test_parser_invalid_finconst_zero_modulus =
  assert_raises_specific_exn Parser.Error (fun () -> Parse.parse_expr "1#0")

let test_parser_invalid_finconst_negative_val =
  assert_raises_specific_exn Parser.Error (fun () -> Parse.parse_expr "-1#5")

let test_parser_invalid_finconst_val_ge_modulus =
  assert_raises_specific_exn Parser.Error (fun () -> Parse.parse_expr "5#5")

(* Interpreter Errors *)
let test_interp_type_error_binop =
  assert_raises_specific_exn (Interp.TypeError "") (fun () -> eval_string "1 + true")

let test_interp_type_error_unop =
  assert_raises_specific_exn (Interp.TypeError "") (fun () -> eval_string "fst 10")

let test_interp_unbound_variable =
  assert_raises_specific_exn (Interp.UnboundVariableError "") (fun () -> eval_string "let x = 1 in y")

let test_interp_invalid_arguments_gaussian =
  assert_raises_specific_exn (Interp.InvalidArgumentsError "") (fun () -> eval_string "gaussian(1.0, 0.0)")

let test_interp_observe_failure =
  assert_raises_specific_exn Interp.ObserveFailure (fun () -> eval_string "observe(false)")


(* --- Tests for Refactored Type Modules (Optional) --- *)

(* TypeSystem Tests *)
let test_typesystem_fresh_meta =
  assert_bool "Fresh metas should be distinct" (fresh_meta () != fresh_meta ())

let test_typesystem_force =
  let m = fresh_meta () in
  assert_equal m (force m) ~msg:"Force on unknown meta should return itself";
  let known_ty = TBool in
  assign (match m with TMeta r -> r | _ -> failwith "Not TMeta") known_ty;
  assert_equal known_ty (force m) ~msg:"Force on known meta should return underlying type"

let test_typesystem_assign_listen =
  let m_ref = match fresh_meta () with TMeta r -> r | _ -> failwith "Not TMeta" in
  let listener_called_with = ref None in
  listen m_ref (fun ty -> listener_called_with := Some ty);
  assert_equal None !listener_called_with ~msg:"Listener should not be called before assign";
  assign m_ref TBool;
  assert_equal (Some TBool) !listener_called_with ~msg:"Listener should be called with assigned type"

(* RuntimeValues Tests *)
let test_runtimevalues_string_of_value =
  assert_equal "true" (string_of_value (VBool true));
  assert_equal "3.14" (string_of_value (VFloat 3.14));
  assert_equal "(true, 3.14)" (string_of_value (VPair (VBool true, VFloat 3.14)));
  assert_equal "2#5" (string_of_value (VFin (2, 5)));
  assert_equal "<fun x>" (string_of_value (VClosure ("x", ExprNode (Const 1.0) , [])));
  assert_equal "()" (string_of_value VUnit);
  assert_equal "[]" (string_of_value VNil);
  assert_equal "[1]" (string_of_value (VCons (VFloat 1.0, VNil)));
  assert_equal "[1; 2]" (string_of_value (VCons (VFloat 1.0, VCons (VFloat 2.0, VNil))));
  assert_equal "ref(true)" (string_of_value (VRef (ref (VBool true))))


(* Test suite *)
let suite =
  "Contdice Comprehensive Tests" >:::
    [
      "test_typing" >:: test_typing;
      (* Lexer Error Tests *)
      "Lexer: Unterminated Comment" >:: test_lexer_unterminated_comment;
      (* Parser Error Tests *)
      "Parser: Invalid FinConst (Zero Modulus)" >:: test_parser_invalid_finconst_zero_modulus;
      "Parser: Invalid FinConst (Negative Value)" >:: test_parser_invalid_finconst_negative_val;
      "Parser: Invalid FinConst (Value >= Modulus)" >:: test_parser_invalid_finconst_val_ge_modulus;
      (* Interpreter Error Tests *)
      "Interpreter: Type Error (BinOp)" >:: test_interp_type_error_binop;
      "Interpreter: Type Error (UnOp)" >:: test_interp_type_error_unop;
      "Interpreter: Unbound Variable" >:: test_interp_unbound_variable;
      "Interpreter: Invalid Arguments (Gaussian)" >:: test_interp_invalid_arguments_gaussian;
      "Interpreter: Observe Failure" >:: test_interp_observe_failure;
      (* Type System Tests *)
      "TypeSystem: Fresh Meta Distinct" >:: test_typesystem_fresh_meta;
      "TypeSystem: Force Meta" >:: test_typesystem_force;
      "TypeSystem: Assign & Listen" >:: test_typesystem_assign_listen;
      (* Runtime Values Tests *)
      "RuntimeValues: String of Value" >:: test_runtimevalues_string_of_value;
    ]

(* Main test runner *)
let () =
  run_test_tt_main suite