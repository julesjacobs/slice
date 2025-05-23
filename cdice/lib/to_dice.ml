(**
 * @file to_dice.ml
 * @brief Converts ContDice ASTs to the Dice probabilistic programming language format.
 *
 * This module provides functionality to translate expressions from the ContDice
 * language (represented by its AST and TAST structures) into equivalent
 * programs in the Dice language (or a Dice-like syntax). This is primarily
 * used for code generation or interoperability.
 *
 * The conversion handles various language constructs, including constants,
 * variables, let bindings, distributions, and control flow, mapping them to
 * their Dice counterparts. Special attention is given to finite set types (`FinConst`)
 * and their representation in Dice using `int(size, value)`.
 *)

open Types (* To access sub-modules like Ast, TypeSystem, etc. *)
open Ast (* For expr, ExprNode, AST constructors, and distribution kinds *)
open TypedAst (* For texpr, TAExprNode (typed AST nodes) *)
open TypeSystem (* For ty and its constructors (type information) *)
open Bags (* For accessing bag internals if needed, though less common in to_dice.ml *)
module Util = Util (* For utility functions like bit_length *)

(**
 * @function extract_fin_modulus
 * @brief Attempts to extract the modulus `k` from a `FinConst (_, k)` expression
 * or the first case of a `DistrCase` if it involves a `FinConst`.
 *
 * This helper is used to determine the size parameter for `int(size, value)`
 * in Dice when translating finite set operations or values.
 *
 * @param e The `Ast.expr` (wrapped in `ExprNode`) to inspect.
 * @return `Some k` if a modulus `k` can be extracted, `None` otherwise.
 *)
let extract_fin_modulus (ExprNode e) : int option =
  match e with
  | FinConst (_, k) -> Some k
  | DistrCase ((ExprNode (FinConst (_, k)), _) :: _) -> Some k
  | _ -> None

(* Forward declarations for mutually recursive pretty-printing/conversion functions.
   These functions convert different parts of the AST/TAST into Dice syntax. *)
let rec string_of_expr_indented ?(indent = 0) e = string_of_expr_node ~indent e
and string_of_aexpr_indented ?(indent = 0) ae = string_of_aexpr_node ~indent ae
and string_of_ty (t : ty) : string = string_of_ty_internal ~seen_metas:[] t (* Wrapper for string_of_ty_internal *)


(**
 * @function string_of_texpr_indented
 * @brief Converts a typed expression (`TypedAst.texpr`) to its Dice string representation,
 * including a type annotation (unless it's a simple `FinConst`).
 *
 * @param indent Current indentation level (primarily for sub-expression formatting if needed, though less used here).
 * @param texpr The typed expression `(ty, aexpr)` to convert.
 * @return String representation in Dice syntax.
 *)
and string_of_texpr_indented ?(indent = 0) ((ty, aexpr) : texpr) : string =
  let aexpr_str = string_of_aexpr_indented ~indent aexpr in
  match aexpr with
  | TAExprNode (FinConst (_, _)) -> aexpr_str (* Avoids ((int(sz,k)) : some_fin_type), just prints int(sz,k) *)
  | _ -> Printf.sprintf "(%s : %s)" aexpr_str (string_of_ty ty) (* General case: (expr : type) *)

(**
 * @function string_of_expr_node
 * @brief Converts an `Ast.expr` (specifically, the `expr_generic` part) to its Dice string representation.
 * This is the core converter for untyped AST nodes to Dice syntax.
 *
 * @param indent Current indentation level for formatting structured code (like let-in, if-then-else).
 * @param expr The `Ast.expr` (specifically, `ExprNode expr_node`) to convert.
 * @return String representation of the AST node in Dice syntax.
 *)
and string_of_expr_node ?(indent = 0) (ExprNode expr_node) : string =
  match expr_node with
  | Const f -> Printf.sprintf "%g" f (* Float constant *)
  | BoolConst b -> string_of_bool b
  | Var x -> x
  | Let (x, e1, e2) -> (
      let indent_str = String.make indent ' ' in
      match e1 with
      | ExprNode (Fun (param, body)) ->
          (* Try to extract int size k from #k from LoopApp inside e2 *)
          let rec find_loopapp_arg e =
            match e with
            | ExprNode (LoopApp (ExprNode (Var f), arg_expr, _)) when f = x ->
                Some arg_expr
            | ExprNode (FuncApp (ExprNode (Var f), arg_expr)) when f = x ->
                Some arg_expr
            | ExprNode (Let (_, e1', e2')) -> (
                match find_loopapp_arg e1' with
                | Some _ as res -> res
                | None -> find_loopapp_arg e2')
            | _ -> None
          in
          let k_opt = Option.bind (find_loopapp_arg e2) extract_fin_modulus in
          let annotation =
            match k_opt with
            | Some k -> Printf.sprintf ": int(%d)" (Util.bit_length (k - 1))
            | None -> ""
          in
          let fun_body_str =
            string_of_expr_indented ~indent:(indent + 2) body
          in
          let rest_str = string_of_expr_indented ~indent e2 in
          Printf.sprintf "%sfun %s(%s%s) {\n%s\n%s}\n%s" indent_str x param
            annotation fun_body_str indent_str rest_str
      | _ ->
          let e1_str = string_of_expr_indented ~indent:(indent + 2) e1 in
          let e2_str = string_of_expr_indented ~indent:(indent + 2) e2 in
          Printf.sprintf "let %s = %s in\n%s%s" x e1_str indent_str e2_str)
  | Sample dist_exp -> string_of_sample ~indent dist_exp
  | DistrCase cases ->
      let format_case (_, prob) =
        match prob with
        | 0. -> "0."
        | 1. -> "1."
        | _ -> Printf.sprintf "%g" prob
      in
      Printf.sprintf "discrete(%s)"
        (String.concat ", " (List.map format_case cases))
  | Less (e1, e2) ->
      Printf.sprintf "%s < %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | LessEq (e1, e2) ->
      Printf.sprintf "%s <= %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | And (e1, e2) ->
      Printf.sprintf "%s && %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Or (e1, e2) ->
      Printf.sprintf "%s || %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Not e1 -> Printf.sprintf "! %s" (string_of_expr_indented ~indent e1)
  | If (e1, e2, e3) ->
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent + 2) ' ' in
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent:(indent + 2) e2 in
      let e3_str = string_of_expr_indented ~indent:(indent + 2) e3 in
      Printf.sprintf "if %s then\n%s%s\n%selse\n%s%s" e1_str next_indent_str
        e2_str indent_str next_indent_str e3_str
  | Pair (e1, e2) ->
      Printf.sprintf "(%s, %s)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | First e -> Printf.sprintf "(fst %s)" (string_of_expr_indented ~indent e)
  | Second e -> Printf.sprintf "(snd %s)" (string_of_expr_indented ~indent e)
  | Fun (x, e) ->
      let e_str = string_of_expr_indented ~indent:(indent + 2) e in
      Printf.sprintf "fun %s -> %s" x e_str
  | FuncApp (e1, e2) ->
      Printf.sprintf "%s(%s)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | LoopApp (e1, e2, n) ->
      Printf.sprintf "iterate(%s,%s,%d)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
        n
  | FinConst (k, n) ->
      if n = 1 then Printf.sprintf "int(%d,%d)" !Util.curr_max_int_sz k
      else Printf.sprintf "int(%d,%d)" (Util.bit_length (n - 1)) k
        (* Printf.sprintf "int(%d,%d)" (Util.bit_length (n-1)) k *)
  | FinLt (e1, e2, _) ->
      Printf.sprintf "%s < %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | FinLeq (e1, e2, _) ->
      Printf.sprintf "%s <= %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | FinEq (e1, e2, _) ->
      Printf.sprintf "%s == %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Observe e1 ->
      Printf.sprintf "observe (%s)" (string_of_expr_indented ~indent e1)
  | Fix (f, x, e) ->
      let e_str = string_of_expr_indented ~indent:(indent + 2) e in
      Printf.sprintf "fix %s %s := %s" f x e_str
  | Nil -> "nil"
  | Cons (e1, e2) ->
      Printf.sprintf "%s :: %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | MatchList (e1, e_nil, y, ys, e_cons) ->
      let e1_str = string_of_expr_indented ~indent:(indent + 2) e1 in
      let e_nil_str = string_of_expr_indented ~indent:(indent + 2) e_nil in
      let e_cons_str = string_of_expr_indented ~indent:(indent + 4) e_cons in
      Printf.sprintf
        "match %s with\n%s  | nil -> %s\n%s  | %s :: %s -> %s\n%send" e1_str
        (String.make indent ' ') e_nil_str (String.make indent ' ') y ys
        e_cons_str (String.make indent ' ')
  | Ref e1 -> Printf.sprintf "(ref %s)" (string_of_expr_indented ~indent e1)
  | Deref e1 -> Printf.sprintf "(!%s)" (string_of_expr_indented ~indent e1)
  | Assign (e1, e2) ->
      Printf.sprintf "(%s := %s)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Seq (e1, e2) ->
      Printf.sprintf "%s; %s"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Unit -> "()"
  | RuntimeError _ -> ""

and string_of_sample ?(indent = 0) dist_exp =
  match dist_exp with
  | Distr2 (DUniform, e1, e2) ->
      Printf.sprintf "uniform(%s, %s)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Distr2 (DGaussian, e1, e2) ->
      Printf.sprintf "gaussian(%s, %s)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Distr1 (DExponential, e1) ->
      Printf.sprintf "exponential(%s)" (string_of_expr_indented ~indent e1)
  | Distr2 (DBeta, e1, e2) ->
      Printf.sprintf "beta(%s, %s)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Distr2 (DLogNormal, e1, e2) ->
      Printf.sprintf "lognormal(%s, %s)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | Distr2 (DGamma, e1, e2) ->
      Printf.sprintf "gamma(%s, %s)"
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)
  | _ -> "<other distribution>"

(**
 * @function string_of_aexpr_node
 * @brief Converts a `TypedAst.aexpr` (specifically, the `expr_generic` part of a TAST node)
 * to its Dice string representation. This function handles the structure of typed AST nodes,
 * deferring to `string_of_texpr_indented` for sub-expressions.
 *
 * @param indent Current indentation level.
 * @param ae The `TypedAst.aexpr` (specifically, `TAExprNode ae_node`) to convert.
 * @return String representation of the TAST node structure in Dice syntax.
 *)
and string_of_aexpr_node ?(indent = 0) (TAExprNode ae_node) : string =
  match ae_node with
  | Const f -> Printf.sprintf "%g" f (* Float constant *)
  | BoolConst b -> string_of_bool b
  | Var x -> x
  | Let (x, te1, te2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_texpr_indented ~indent:(indent + 2) te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent + 2) te2 in
      Printf.sprintf "let %s = %s in\n%s%s" x e1_str indent_str e2_str
  | Sample dist_exp -> string_of_asample dist_exp
  | DistrCase cases ->
      let format_case (_, prob) =
        match prob with
        | 0. -> "0."
        | 1. -> "1."
        | _ -> Printf.sprintf "%g" prob
      in
      Printf.sprintf "discrete(%s)"
        (String.concat ", " (List.map format_case cases))
  | Less (te1, te2) ->
      Printf.sprintf "%s < %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | LessEq (te1, te2) ->
      Printf.sprintf "%s <= %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | And (te1, te2) ->
      Printf.sprintf "%s && %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | Or (te1, te2) ->
      Printf.sprintf "%s || %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | Not te1 -> Printf.sprintf "! %s" (string_of_texpr_indented ~indent te1)
  | If (te1, te2, te3) ->
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent + 2) ' ' in
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent + 2) te2 in
      let e3_str = string_of_texpr_indented ~indent:(indent + 2) te3 in
      Printf.sprintf "if %s then\n%s%s\n%selse\n%s%s" e1_str next_indent_str
        e2_str indent_str next_indent_str e3_str
  | Pair (te1, te2) ->
      Printf.sprintf "(%s, %s)"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | First te -> Printf.sprintf "(fst %s)" (string_of_texpr_indented ~indent te)
  | Second te -> Printf.sprintf "(snd %s)" (string_of_texpr_indented ~indent te)
  | Fun (x, te) ->
      let e_str = string_of_texpr_indented ~indent:(indent + 2) te in
      Printf.sprintf "fun %s -> %s" x e_str
  | FuncApp (te1, te2) ->
      Printf.sprintf "%s(%s)"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | LoopApp (te1, te2, _) ->
      Printf.sprintf "(%s %s)"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | FinConst (k, n) -> Printf.sprintf "int(%d,%d)" (Util.bit_length (n - 1)) k
  | FinLt (te1, te2, _) ->
      Printf.sprintf "%s < %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | FinLeq (te1, te2, _) ->
      Printf.sprintf "%s <= %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | FinEq (te1, te2, _) ->
      Printf.sprintf "%s == %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | Observe te1 ->
      Printf.sprintf "observe (%s)" (string_of_texpr_indented ~indent te1)
  | Fix (f, x, te) ->
      let te_str = string_of_texpr_indented ~indent:(indent + 2) te in
      Printf.sprintf "fix %s %s := %s" f x te_str
  | Nil -> "nil"
  | Cons (te1, te2) ->
      Printf.sprintf "%s :: %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | MatchList (te1, te_nil, y, ys, te_cons) ->
      let te1_str = string_of_texpr_indented ~indent:(indent + 2) te1 in
      let te_nil_str = string_of_texpr_indented ~indent:(indent + 2) te_nil in
      let te_cons_str = string_of_texpr_indented ~indent:(indent + 4) te_cons in
      Printf.sprintf
        "match %s with\n%s  | nil -> %s\n%s  | %s :: %s -> %s\n%send" te1_str
        (String.make indent ' ') te_nil_str (String.make indent ' ') y ys
        te_cons_str (String.make indent ' ')
  | Ref te1 -> Printf.sprintf "(ref %s)" (string_of_texpr_indented ~indent te1)
  | Deref te1 -> Printf.sprintf "(!%s)" (string_of_texpr_indented ~indent te1)
  | Assign (te1, te2) ->
      Printf.sprintf "(%s := %s)"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | Seq (te1, te2) ->
      Printf.sprintf "%s; %s"
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)
  | Unit -> "()"
  | RuntimeError _ -> ""

and string_of_asample ?(indent = 0) dist_exp =
  match dist_exp with
  | Distr2 (DUniform, e1, e2) ->
      Printf.sprintf "uniform(%s, %s)"
        (string_of_texpr_indented ~indent e1)
        (string_of_texpr_indented ~indent e2)
  | Distr2 (DGaussian, e1, e2) ->
      Printf.sprintf "gaussian(%s, %s)"
        (string_of_texpr_indented ~indent e1)
        (string_of_texpr_indented ~indent e2)
  | Distr1 (DExponential, e1) ->
      Printf.sprintf "exponential(%s)" (string_of_texpr_indented ~indent e1)
  | Distr2 (DBeta, e1, e2) ->
      Printf.sprintf "beta(%s, %s)"
        (string_of_texpr_indented ~indent e1)
        (string_of_texpr_indented ~indent e2)
  | Distr2 (DLogNormal, e1, e2) ->
      Printf.sprintf "lognormal(%s, %s)"
        (string_of_texpr_indented ~indent e1)
        (string_of_texpr_indented ~indent e2)
  | Distr2 (DGamma, e1, e2) ->
      Printf.sprintf "gamma(%s, %s)"
        (string_of_texpr_indented ~indent e1)
        (string_of_texpr_indented ~indent e2)
  | _ -> "<other distribution>"

(**
 * @function string_of_ty_internal
 * @brief Internal converter for `TypeSystem.ty` types to their Dice string representation.
 * Handles recursive types by tracking seen meta variables.
 *
 * @param seen_metas List of `meta_ref` already encountered to detect recursion.
 * @param t The type to convert.
 * @return String representation of the type in Dice syntax (or a simplified version).
 *)
and string_of_ty_internal ~seen_metas (t : ty) : string =
  match t with
  | TBool -> "bool"
  | TFloat (_bound_bag_ref, _const_bag_ref) -> "float" (* Simplified for Dice; bag details not usually in Dice type syntax *)
  | TPair (t1, t2) ->
      Printf.sprintf "(%s * %s)" (string_of_ty_internal ~seen_metas t1) (string_of_ty_internal ~seen_metas t2)
  | TFun (t1, t2) ->
      Printf.sprintf "(%s -> %s)" (string_of_ty_internal ~seen_metas t1) (string_of_ty_internal ~seen_metas t2)
  | TFin n -> Printf.sprintf "int(%d)" (Util.bit_length (n-1)) (* Dice `int(size)` type, size from modulus *)
  | TUnit -> "()" (* Dice often uses `()` or specific unit type, this is a common representation *)
  | TList t -> Printf.sprintf "list<%s>" (string_of_ty_internal ~seen_metas t) (* Generic list type syntax *)
  | TRef t -> Printf.sprintf "ref<%s>" (string_of_ty_internal ~seen_metas t)  (* Generic ref type syntax *)
  | TMeta r ->
      if List.exists (funmr -> mr == r) seen_metas then
        "?rec_ty" (* Recursive type variable detected *)
      else
        (match !r with
         | Known t' -> string_of_ty_internal ~seen_metas:(r :: seen_metas) t' (* Known, print underlying type *)
         | Unknown _ -> "?ty") (* Unknown type variable *)

(** Public wrapper for `string_of_expr_indented` for converting an expression to Dice string. *)
let string_of_expr expr = string_of_expr_indented expr

(** Public wrapper for `string_of_texpr_indented` for converting a typed expression to Dice string. *)
let string_of_texpr texpr = string_of_texpr_indented texpr

(** Public wrapper for `string_of_aexpr_indented` for converting an annotated (typed) AST node to Dice string. *)
let string_of_aexpr aexpr = string_of_aexpr_indented aexpr
