open OUnit2

(* Test typechecking *)
let test_typing _test_ctxt =
  let expr = Slice.Parse.parse_expr "let x = uniform(0, 1) in if x < 0.5 then 0 else 1" in
  try
    let texpr = Slice.Inference.infer expr in
    ignore (Slice.Pretty.string_of_texpr texpr)
  with e -> 
    assert_failure ("Type check test failed: " ^ Printexc.to_string e)

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

(* Test suite *) 
let suite = 
  "Slice Tests" >:::
    [(* Removed test_parse and test_pretty_print *)
     "test_typing" >:: test_typing;
     "test_fix_substitution_freshens_binders" >:: test_fix_substitution_freshens_binders;
     "test_match_substitution_freshens_binders" >:: test_match_substitution_freshens_binders;
    ]

(* Main test runner *) 
let () = 
  run_test_tt_main suite
