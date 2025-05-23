(**
 * @file pretty.ml
 * @brief Pretty-printing utilities for ContDice ASTs, types, and conversion to SPPL.
 *
 * This module provides functions to convert ContDice Abstract Syntax Trees (ASTs),
 * Typed Abstract Syntax Trees (TASTs), and type definitions (`TypeSystem.ty`)
 * into human-readable string representations. It includes support for ANSI color
 * codes for syntax highlighting in the terminal.
 *
 * Additionally, this module contains logic for translating a subset of ContDice
 * expressions into the SPPL (Statistical Probabilistic Programming Language) format.
 *)

open Types (* To access sub-modules like Ast, TypeSystem, etc. *)
open Ast (* For expr, ExprNode, and AST constructors *)
open TypedAst (* For texpr, aexpr, TAExprNode *)
open TypeSystem (* For ty and its constructors *)
(* RuntimeValues is not directly used for AST/type printing but might be relevant if printing values. *)
open Bags (* For accessing Bag internals when printing TFloat details. *)

(** ANSI escape codes for colorizing terminal output. *)
let keyword_color = "\027[1;34m" (* Bold Blue for keywords like let, if, fun *)
let operator_color = "\027[1;31m" (* Bold Red for operators like <, &&, -> *)
let number_color = "\027[0;32m" (* Green for numeric literals *)
let variable_color = "\027[0;33m" (* Yellow for variable identifiers *)
let reset_color = "\027[0m" (* Resets all ANSI formatting to default *)
let paren_color = "\027[1;37m" (* Bold White for parentheses and other delimiters *)
let type_color = "\027[1;35m" (* Bold Magenta for type annotations and type names *)
let bracket_color = "\027[1;36m" (* Bold Cyan for brackets in types or tuples *)

(**
 * @function string_of_cdistr
 * @brief Converts a `Distributions.cdistr` runtime value to a string with syntax highlighting.
 * @param dist The distribution value from the `Distributions` module.
 * @return A string representation of the distribution.
 *)
let string_of_cdistr = function
  | Distributions.Uniform (lo, hi) ->
      Printf.sprintf "%suniform%s(%s%g%s, %s%g%s)" keyword_color reset_color (* e.g., uniform(0.0, 1.0) *)
        number_color lo reset_color number_color hi reset_color
  | Distributions.Gaussian (mean, std) ->
      Printf.sprintf "%sgaussian%s(%s%g%s, %s%g%s)" keyword_color reset_color
        number_color mean reset_color number_color std reset_color
  | Distributions.Exponential rate ->
      Printf.sprintf "%sexponential%s(%s%g%s)" keyword_color reset_color
        number_color rate reset_color
  | Distributions.Beta (alpha, beta) ->
      Printf.sprintf "%sbeta%s(%s%g%s, %s%g%s)" keyword_color reset_color
        number_color alpha reset_color number_color beta reset_color
  | Distributions.LogNormal (mu, sigma) ->
      Printf.sprintf "%slognormal%s(%s%g%s, %s%g%s)" keyword_color reset_color
        number_color mu reset_color number_color sigma reset_color
  | Distributions.Gamma (shape, scale) ->
      Printf.sprintf "%sgamma%s(%s%g%s, %s%g%s)" keyword_color reset_color
        number_color shape reset_color number_color scale reset_color
  | Distributions.Laplace scale ->
      Printf.sprintf "%slaplace%s(%s%g%s)" keyword_color reset_color
        number_color scale reset_color
  | Distributions.Cauchy scale ->
      Printf.sprintf "%scauchy%s(%s%g%s)" keyword_color reset_color number_color
        scale reset_color
  | Distributions.Weibull (a, b) ->
      Printf.sprintf "%sweibull%s(%s%g%s, %s%g%s)" keyword_color reset_color
        number_color a reset_color number_color b reset_color
  | Distributions.TDist nu ->
      Printf.sprintf "%stdist%s(%s%g%s)" keyword_color reset_color number_color
        nu reset_color
  | Distributions.Chi2 nu ->
      Printf.sprintf "%schi2%s(%s%g%s)" keyword_color reset_color number_color
        nu reset_color
  | Distributions.Logistic scale ->
      Printf.sprintf "%slogistic%s(%s%g%s)" keyword_color reset_color
        number_color scale reset_color
  | _ -> "<unsupported distribution>"

(* Forward declarations for mutually recursive pretty-printing functions. *)
let rec string_of_expr_indented ?(indent = 0) e = string_of_expr_node ~indent e
and string_of_aexpr_indented ?(indent = 0) ae = string_of_aexpr_node ~indent ae
and string_of_ty (t : ty) : string = string_of_ty_internal ~seen_metas:[] t (* Wrapper for string_of_ty_internal *)

(**
 * @function string_of_texpr_indented
 * @brief Converts a typed expression (`TypedAst.texpr`) to a string with indentation and type annotation.
 * Special case: `FinConst` nodes are printed without the redundant type annotation if it's clear.
 * @param indent Current indentation level (number of spaces).
 * @param texpr The typed expression `(ty, aexpr)` to print.
 * @return String representation of the typed expression.
 *)
and string_of_texpr_indented ?(indent = 0) ((ty, aexpr) : texpr) : string =
  let aexpr_str = string_of_aexpr_indented ~indent aexpr in
  match aexpr with
  | TAExprNode (FinConst (_, _)) -> aexpr_str (* Avoids ((k#n) : #n), just prints k#n *)
  | _ ->
      (* Default: print expression with its type, e.g., (expr_string : type_string) *)
      Printf.sprintf "%s(%s%s : %s%s)%s" paren_color reset_color aexpr_str
        (string_of_ty ty) paren_color reset_color

(**
 * @function string_of_expr_node
 * @brief Converts an `Ast.expr` (specifically, the `expr_generic` part) to a string with indentation.
 * This is the core pretty-printer for untyped AST nodes.
 * @param indent Current indentation level.
 * @param expr The `Ast.expr` (specifically, `ExprNode expr_node`) to print.
 * @return String representation of the AST node.
 *)
and string_of_expr_node ?(indent = 0) (ExprNode expr_node) : string =
  match expr_node with
  | Const f -> Printf.sprintf "%s%g%s" number_color f reset_color (* e.g., 3.14 *)
  | BoolConst b -> Printf.sprintf "%s%b%s" keyword_color b reset_color
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, e1, e2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_expr_indented ~indent:(indent + 2) e1 in
      let e2_str = string_of_expr_indented ~indent:(indent + 2) e2 in
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s" keyword_color
        reset_color variable_color x reset_color e1_str keyword_color
        reset_color indent_str e2_str
  | Sample dist_exp -> string_of_sample ~indent dist_exp
  | DistrCase cases ->
      let format_case (expr, prob) =
        Printf.sprintf "%s%g%s: %s" number_color prob reset_color
          (string_of_expr_indented ~indent expr)
      in
      Printf.sprintf "%sdiscrete%s(%s%s%s)" keyword_color reset_color
        paren_color
        (String.concat ", " (List.map format_case cases))
        reset_color
  | Less (e1, e2) ->
      Printf.sprintf "%s %s<%s %s"
        (string_of_expr_indented ~indent e1)
        operator_color reset_color
        (string_of_expr_indented ~indent e2)
  | LessEq (e1, e2) ->
      Printf.sprintf "%s %s<=%s %s"
        (string_of_expr_indented ~indent e1)
        operator_color reset_color
        (string_of_expr_indented ~indent e2)
  | And (e1, e2) ->
      Printf.sprintf "%s %s&&%s %s"
        (string_of_expr_indented ~indent e1)
        operator_color reset_color
        (string_of_expr_indented ~indent e2)
  | Or (e1, e2) ->
      Printf.sprintf "%s %s||%s %s"
        (string_of_expr_indented ~indent e1)
        operator_color reset_color
        (string_of_expr_indented ~indent e2)
  | Not e1 ->
      Printf.sprintf "(%snot%s %s)" operator_color reset_color
        (string_of_expr_indented ~indent e1)
  | If (e1, e2, e3) ->
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent + 2) ' ' in
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent:(indent + 2) e2 in
      let e3_str = string_of_expr_indented ~indent:(indent + 2) e3 in
      Printf.sprintf "%sif%s %s %sthen%s\n%s%s\n%s%selse%s\n%s%s" keyword_color
        reset_color e1_str keyword_color reset_color next_indent_str e2_str
        indent_str keyword_color reset_color next_indent_str e3_str
  | Pair (e1, e2) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "(%s, %s)" e1_str e2_str
  | First e ->
      let e_str = string_of_expr_indented ~indent e in
      Printf.sprintf "(%sfst%s %s)" keyword_color reset_color e_str
  | Second e ->
      let e_str = string_of_expr_indented ~indent e in
      Printf.sprintf "(%ssnd%s %s)" keyword_color reset_color e_str
  | Fun (x, e) ->
      let e_str = string_of_expr_indented ~indent:(indent + 2) e in
      Printf.sprintf "%sfun%s %s%s%s %s->%s %s" keyword_color reset_color
        variable_color x reset_color operator_color reset_color e_str
  | FuncApp (e1, e2) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "(%s %s)" e1_str e2_str
  | LoopApp (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "(%s %s %d)" e1_str e2_str n
  | Fix (f, x, e) ->
      let e_str = string_of_expr_indented ~indent:(indent + 2) e in
      Printf.sprintf "%sfix%s %s%s%s %s%s%s %s:=%s %s" keyword_color reset_color
        variable_color f reset_color variable_color x reset_color operator_color
        reset_color e_str
  | FinConst (k, n) ->
      Printf.sprintf "%s%d%s%s#%d%s" number_color k reset_color type_color n
        reset_color
  | FinLt (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s<%s%s#%d%s %s" e1_str operator_color reset_color
        type_color n reset_color e2_str
  | FinLeq (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s<=%s%s#%d%s %s" e1_str operator_color reset_color
        type_color n reset_color e2_str
  | FinEq (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s==#%s%s#%d%s %s" e1_str operator_color reset_color
        type_color n reset_color e2_str
  | Observe e1 ->
      let e1_str = string_of_expr_indented ~indent e1 in
      Printf.sprintf "%sobserve%s (%s)" keyword_color reset_color e1_str
  | Nil -> Printf.sprintf "%snil%s" keyword_color reset_color
  | Cons (e1, e2) ->
      Printf.sprintf "%s %s::%s %s"
        (string_of_expr_indented ~indent e1)
        operator_color reset_color
        (string_of_expr_indented ~indent e2)
  | MatchList (e1, e_nil, y, ys, e_cons) ->
      let e1_str = string_of_expr_indented ~indent:(indent + 2) e1 in
      let e_nil_str = string_of_expr_indented ~indent:(indent + 2) e_nil in
      let e_cons_str = string_of_expr_indented ~indent:(indent + 4) e_cons in
      Printf.sprintf
        "%smatch%s %s %swith%s\n\
         %s  | %snil%s %s->%s %s\n\
         %s  | %s%s%s %s::%s %s%s%s %s->%s %s\n\
         %s%send%s"
        keyword_color reset_color e1_str keyword_color reset_color
        (String.make indent ' ') keyword_color reset_color operator_color
        reset_color e_nil_str (String.make indent ' ') variable_color y
        reset_color operator_color reset_color variable_color ys reset_color
        operator_color reset_color e_cons_str (String.make indent ' ')
        keyword_color reset_color
  | Ref e1 ->
      Printf.sprintf "%sref%s %s" keyword_color reset_color
        (string_of_expr_indented ~indent e1)
  | Deref e1 ->
      Printf.sprintf "%s!%s%s" operator_color reset_color
        (string_of_expr_indented ~indent e1)
  | Assign (e1, e2) ->
      Printf.sprintf "%s %s:=%s %s"
        (string_of_expr_indented ~indent e1)
        operator_color reset_color
        (string_of_expr_indented ~indent e2)
  | Seq (e1, e2) ->
      Printf.sprintf "%s %s;%s %s"
        (string_of_expr_indented ~indent e1)
        operator_color reset_color
        (string_of_expr_indented ~indent e2)
  | Unit -> Printf.sprintf "%s()%s" keyword_color reset_color
  | RuntimeError s ->
      Printf.sprintf "%sRUNTIME_ERROR%s(\"%s%s%s\")" operator_color reset_color
        variable_color s reset_color

and string_of_sample ?(indent = 0) dist_exp =
  match dist_exp with
  | Distr1 (kind, e1) ->
      Printf.sprintf "%s%s%s(%s)" keyword_color
        (Distributions.string_of_single_arg_dist_kind kind)
        reset_color
        (string_of_expr_indented ~indent e1)
  | Distr2 (kind, e1, e2) ->
      Printf.sprintf "%s%s%s(%s, %s)" keyword_color
        (Distributions.string_of_two_arg_dist_kind kind)
        reset_color
        (string_of_expr_indented ~indent e1)
        (string_of_expr_indented ~indent e2)

(**
 * @function string_of_aexpr_node
 * @brief Converts a `TypedAst.aexpr` (specifically, the `expr_generic` part of a TAST node) to a string.
 * This is the core pretty-printer for the structure of typed AST nodes, deferring to
 * `string_of_texpr_indented` for sub-expressions (which includes their types).
 * @param indent Current indentation level.
 * @param ae The `TypedAst.aexpr` (specifically, `TAExprNode ae_node`) to print.
 * @return String representation of the TAST node structure.
 *)
and string_of_aexpr_node ?(indent = 0) (TAExprNode ae_node) : string =
  match ae_node with
  | Const f -> Printf.sprintf "%s%g%s" number_color f reset_color (* e.g., 3.14 *)
  | BoolConst b -> Printf.sprintf "%s%b%s" keyword_color b reset_color
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, te1, te2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_texpr_indented ~indent:(indent + 2) te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent + 2) te2 in
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s" keyword_color
        reset_color variable_color x reset_color e1_str keyword_color
        reset_color indent_str e2_str
  | Sample dist_exp -> string_of_asample ~indent dist_exp
  | DistrCase cases ->
      let format_case (texpr, prob) =
        Printf.sprintf "%s%g%s: %s" number_color prob reset_color
          (string_of_texpr_indented ~indent texpr)
      in
      Printf.sprintf "%sdiscrete%s(%s%s%s)" keyword_color reset_color
        paren_color
        (String.concat ", " (List.map format_case cases))
        reset_color
  | Less (te1, te2) ->
      Printf.sprintf "%s %s<%s %s"
        (string_of_texpr_indented ~indent te1)
        operator_color reset_color
        (string_of_texpr_indented ~indent te2)
  | LessEq (te1, te2) ->
      Printf.sprintf "%s %s<=%s %s"
        (string_of_texpr_indented ~indent te1)
        operator_color reset_color
        (string_of_texpr_indented ~indent te2)
  | And (te1, te2) ->
      Printf.sprintf "%s %s&&%s %s"
        (string_of_texpr_indented ~indent te1)
        operator_color reset_color
        (string_of_texpr_indented ~indent te2)
  | Or (te1, te2) ->
      Printf.sprintf "%s %s||%s %s"
        (string_of_texpr_indented ~indent te1)
        operator_color reset_color
        (string_of_texpr_indented ~indent te2)
  | Not te1 ->
      Printf.sprintf "(%snot%s %s)" operator_color reset_color
        (string_of_texpr_indented ~indent te1)
  | If (te1, te2, te3) ->
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent + 2) ' ' in
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent + 2) te2 in
      let e3_str = string_of_texpr_indented ~indent:(indent + 2) te3 in
      Printf.sprintf "%sif%s %s %sthen%s\n%s%s\n%s%selse%s\n%s%s" keyword_color
        reset_color e1_str keyword_color reset_color next_indent_str e2_str
        indent_str keyword_color reset_color next_indent_str e3_str
  | Pair (te1, te2) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "(%s, %s)" e1_str e2_str
  | First te ->
      let e_str = string_of_texpr_indented ~indent te in
      Printf.sprintf "(%sfst%s %s)" keyword_color reset_color e_str
  | Second te ->
      let e_str = string_of_texpr_indented ~indent te in
      Printf.sprintf "(%ssnd%s %s)" keyword_color reset_color e_str
  | Fun (x, te) ->
      let e_str = string_of_texpr_indented ~indent:(indent + 2) te in
      Printf.sprintf "%sfun%s %s%s%s %s->%s %s" keyword_color reset_color
        variable_color x reset_color operator_color reset_color e_str
  | FuncApp (te1, te2) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "(%s %s)" e1_str e2_str
  | LoopApp (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "(%s %s %d)" e1_str e2_str n
  | Fix (f, x, te) ->
      let te_str = string_of_texpr_indented ~indent:(indent + 2) te in
      Printf.sprintf "%sfix%s %s%s%s %s%s%s %s:=%s %s" keyword_color reset_color
        variable_color f reset_color variable_color x reset_color operator_color
        reset_color te_str
  | FinConst (k, n) ->
      Printf.sprintf "%s%d%s%s#%d%s" number_color k reset_color type_color n
        reset_color
  | FinLt (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s<%s%s#%d%s %s" e1_str operator_color reset_color
        type_color n reset_color e2_str
  | FinLeq (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s<=%s%s#%d%s %s" e1_str operator_color reset_color
        type_color n reset_color e2_str
  | FinEq (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s==#%s%s#%d%s %s" e1_str operator_color reset_color
        type_color n reset_color e2_str
  | Observe te1 ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      Printf.sprintf "%sobserve%s (%s)" keyword_color reset_color e1_str
  | Nil -> Printf.sprintf "%snil%s" keyword_color reset_color
  | Cons (te1, te2) ->
      Printf.sprintf "%s %s::%s %s"
        (string_of_texpr_indented ~indent te1)
        operator_color reset_color
        (string_of_texpr_indented ~indent te2)
  | MatchList (te1, te_nil, y, ys, te_cons) ->
      let te1_str = string_of_texpr_indented ~indent:(indent + 2) te1 in
      let te_nil_str = string_of_texpr_indented ~indent:(indent + 2) te_nil in
      let te_cons_str = string_of_texpr_indented ~indent:(indent + 4) te_cons in
      Printf.sprintf
        "%smatch%s %s %swith%s\n\
         %s  | %snil%s %s->%s %s\n\
         %s  | %s%s%s %s::%s %s%s%s %s->%s %s\n\
         %s%send%s"
        keyword_color reset_color te1_str keyword_color reset_color
        (String.make indent ' ') keyword_color reset_color operator_color
        reset_color te_nil_str (String.make indent ' ') variable_color y
        reset_color operator_color reset_color variable_color ys reset_color
        operator_color reset_color te_cons_str (String.make indent ' ')
        keyword_color reset_color
  | Ref te1 ->
      Printf.sprintf "%sref%s %s" keyword_color reset_color
        (string_of_texpr_indented ~indent te1)
  | Deref te1 ->
      Printf.sprintf "%s!%s%s" operator_color reset_color
        (string_of_texpr_indented ~indent te1)
  | Assign (te1, te2) ->
      Printf.sprintf "%s %s:=%s %s"
        (string_of_texpr_indented ~indent te1)
        operator_color reset_color
        (string_of_texpr_indented ~indent te2)
  | Seq (te1, te2) ->
      Printf.sprintf "%s %s;%s %s"
        (string_of_texpr_indented ~indent te1)
        operator_color reset_color
        (string_of_texpr_indented ~indent te2)
  | Unit -> Printf.sprintf "%s()%s" keyword_color reset_color
  | RuntimeError s ->
      Printf.sprintf "%sRUNTIME_ERROR%s(\"%s%s%s\")" operator_color reset_color
        variable_color s reset_color

(**
 * @function string_of_ty_internal
 * @brief Internal pretty-printer for `TypeSystem.ty` types.
 * Handles recursive types (like `TMeta` pointing to itself) by keeping track
 * of seen meta variables to avoid infinite loops, printing "?" for such cases.
 * @param seen_metas A list of `meta_ref` already encountered in the current printing path.
 * @param t The type to print.
 * @return String representation of the type.
 *)
and string_of_ty_internal ~seen_metas (t : ty) : string =
  match t with
  | TBool -> Printf.sprintf "%sbool%s" type_color reset_color
  | TFloat (bound_bag_ref, const_bag_ref) ->
      (* Helper to format bag contents (bounds or constants) *)
      let format_bag_content to_string_func get_bag_func bag_ref default_if_empty =
        match get_bag_func bag_ref with
        | Top -> "T" (* Top is represented as 'T' *)
        | Finite set ->
            if Set.Make(struct type t = float let compare = compare end).is_empty set && (match bag_ref with FloatBag.bag _ -> true | _ -> false) then default_if_empty
            else if Set.Make(BoundOrder).is_empty set && (match bag_ref with BoundBag.bag _ -> true | _ -> false) then default_if_empty
            else
              let elements = 
                if (match bag_ref with FloatBag.bag _ -> true | _ -> false) then
                    List.map (Printf.sprintf "%g") (FloatSet.elements set)
                else (* Assuming BoundBag *)
                    List.map to_string_func (BoundSet.elements set)
              in
              String.concat "," elements
      in
      let string_of_bound = function | Bags.Less c -> Printf.sprintf "<%g" c | Bags.LessEq c -> Printf.sprintf "<=%g" c in
      let bounds_str = format_bag_content string_of_bound BoundBag.get bound_bag_ref "" in
      let consts_str = format_bag_content (Printf.sprintf "%g") FloatBag.get const_bag_ref "" in
      
      let content_str =
        match (bounds_str, consts_str) with
        | "", "" -> "" (* Just "float" *)
        | b, "" -> Printf.sprintf "%s[%s]%s" type_color b reset_color (* float[bounds] *)
        | "", c -> Printf.sprintf "%s[; %s]%s" type_color c reset_color (* float[; consts] *)
        | b, c -> Printf.sprintf "%s[%s; %s]%s" type_color b c reset_color (* float[bounds; consts] *)
      in
      Printf.sprintf "%sfloat%s%s" type_color reset_color content_str
  | TPair (t1, t2) ->
      Printf.sprintf "%s(%s * %s)%s" bracket_color (string_of_ty_internal ~seen_metas t1)
        (string_of_ty_internal ~seen_metas t2) reset_color
  | TFun (t1, t2) ->
      Printf.sprintf "%s(%s -> %s)%s" bracket_color (string_of_ty_internal ~seen_metas t1)
        (string_of_ty_internal ~seen_metas t2) reset_color
  | TFin n -> Printf.sprintf "%s#%d%s" type_color n reset_color (* e.g., #5 *)
  | TUnit -> Printf.sprintf "%sunit%s" type_color reset_color
  | TList t ->
      Printf.sprintf "%slist%s %s" type_color reset_color (string_of_ty_internal ~seen_metas t)
  | TRef t ->
      Printf.sprintf "%s%s ref%s" type_color (string_of_ty_internal ~seen_metas t) reset_color
  | TMeta r ->
      if List.exists (funmr -> mr == r) seen_metas then
        Printf.sprintf "%s?recursion%s" type_color reset_color (* Already seen this meta_ref, indicate recursion *)
      else
        (match !r with
         | Known t' -> string_of_ty_internal ~seen_metas:(r :: seen_metas) t' (* Known, print the underlying type *)
         | Unknown _ -> Printf.sprintf "%s?%s" type_color reset_color) (* Unknown, print '?' *)

(**
 * @function string_of_asample
 * @brief Converts a typed distribution sample (`'a sample` where 'a is `texpr`) to a string.
 * @param indent Current indentation level.
 * @param dist_exp The typed distribution sample expression.
 * @return String representation.
 *)
and string_of_asample ?(indent = 0) dist_exp =
  match dist_exp with
  | Distr1 (kind, te1) ->
      Printf.sprintf "%s%s%s(%s)" keyword_color
        (Distributions.string_of_single_arg_dist_kind kind)
        reset_color
        (string_of_texpr_indented ~indent te1)
  | Distr2 (kind, te1, te2) ->
      Printf.sprintf "%s%s%s(%s, %s)" keyword_color
        (Distributions.string_of_two_arg_dist_kind kind)
        reset_color
        (string_of_texpr_indented ~indent te1)
        (string_of_texpr_indented ~indent te2)

(* Public wrapper functions for the pretty-printers. *)
let string_of_expr expr = string_of_expr_indented expr
let string_of_texpr texpr = string_of_texpr_indented texpr
let string_of_aexpr aexpr = string_of_aexpr_indented aexpr

(**
 * @function string_of_float_list
 * @brief Converts a list of floats to a string representation like `[f1; f2; ...]`.
 * @param l The list of floats.
 * @return String representation of the float list.
 *)
let string_of_float_list (l : float list) : string =
  "[" ^ String.concat "; " (List.map (Printf.sprintf "%g") l) ^ "]"

(* ================================================================================ *)
(* SPPL (Statistical Probabilistic Programming Language) Conversion Logic           *)
(* ================================================================================ *)

(** State for generating unique variable names during SPPL translation (e.g., x1, x2). *)
type sppl_state = { mutable next_var : int }

(** Generates a fresh variable name for SPPL code. *)
let fresh_sppl_var state =
  state.next_var <- state.next_var + 1;
  Printf.sprintf "x%d" state.next_var

(** Checks if a string is a "simple variable" generated by `fresh_sppl_var`.
    This is used to optimize SPPL `let` bindings: if an expression result is already
    a simple variable, an extra assignment can be avoided. *)
let is_simple_var s =
  String.length s > 0
  && s.[0] = 'x'
  &&
  try
    ignore (int_of_string (String.sub s 1 (String.length s - 1)));
    true
  with Failure _ -> false

(**
 * @function translate_to_sppl
 * @brief Recursively translates a ContDice expression (AST) into a list of SPPL statements and the name of the SPPL variable holding the result.
 *
 * This is the core of the SPPL conversion. It handles different AST nodes and maps them
 * to corresponding SPPL constructs.
 *
 * @param env A list mapping ContDice variable names to their SPPL variable names.
 * @param target_var An optional string. If `Some name`, the function tries to assign the result of the current expression to SPPL variable `name`.
 * @param expr The ContDice expression (`Ast.expr`) to translate.
 * @param state The SPPL state for generating fresh variable names.
 * @return A pair: `(sppl_statements_list, result_sppl_variable_name)`.
 *)
let rec translate_to_sppl (env : (string * string) list)
    ?(target_var : string option = None) (expr : expr)
    (state : sppl_state) : string list * string =
  match expr with
  (* Base Cases: Constants and Variables *)
  | ExprNode (Const f) ->
      let val_str = string_of_float f in
      (match target_var with
      | Some name -> ([ Printf.sprintf "%s = %s" name val_str ], name) (* Assign to target if provided *)
      | None -> ([], val_str)) (* Otherwise, return the constant string itself (used if it's an argument to another op) *)
  | ExprNode (BoolConst b) ->
      let val_str = string_of_bool b |> String.capitalize_ascii in (* SPPL uses True/False *)
      (match target_var with
      | Some name -> ([ Printf.sprintf "%s = %s" name val_str ], name)
      | None -> ([], val_str))
  | ExprNode (Var x) ->
      let var_name =
        try List.assoc x env (* Look up ContDice var name in SPPL environment *)
        with Not_found -> failwith ("Unbound variable during SPPL translation: " ^ x)
      in
      (match target_var with
      | Some name when name <> var_name -> ([ Printf.sprintf "%s = %s" name var_name ], name) (* Assign if target is different *)
      | Some name (* when name = var_name *) -> ([], name) (* No assignment needed if target is already the var *)
      | None -> ([], var_name)) (* No target, just return the SPPL var name *)

  (* Sampling from Distributions: Result must be assigned to a variable in SPPL. *)
  | ExprNode (Sample d) ->
      let assign_var = target_var |? (fresh_sppl_var state) in (* Use target_var or a fresh one *)
      (* Helper to ensure distribution parameters are constants, as SPPL often requires this. *)
      let assert_float_const e_param =
        match e_param with
        | ExprNode (Const f_val) -> f_val
        | _ -> failwith "SPPL translation expects constant parameters for distributions (in pretty.ml)"
      in
      let stmt = (* Construct the SPPL sampling statement based on distribution type *)
        match d with
        | Distr2 (DUniform, a_expr, b_expr) ->
            let a_val = assert_float_const a_expr in
            let b_val = assert_float_const b_expr in
            Printf.sprintf "%s ~= uniform(loc=%f, scale=%f)" assign_var a_val (b_val -. a_val) (* SPPL uniform: loc=low, scale=width *)
        | Distr2 (DGaussian, mu, sigma) ->
            let mu = assert_float_const mu in
            let sigma = assert_float_const sigma in
            Printf.sprintf "%s ~= normal(loc=%f, scale=%f)" assign_var mu sigma
        | Distr1 (DExponential, rate) ->
            let rate = assert_float_const rate in
            Printf.sprintf "%s ~= exponential(scale=%f)" assign_var (1.0 /. rate)
            (* SPPL scale = 1/rate *)
        | Distr2 (DBeta, alpha, beta_param) ->
            let alpha = assert_float_const alpha in
            let beta_val = assert_float_const beta_param in
            Printf.sprintf "%s ~= beta(a=%f, b=%f)" assign_var alpha beta_val
        | Distr2 (DLogNormal, mu, sigma) ->
            let mu = assert_float_const mu in
            let sigma = assert_float_const sigma in
            Printf.sprintf "%s ~= lognormal(mu=%f, sigma=%f)" assign_var mu
              sigma
        | Distr2 (DGamma, shape, scale) ->
            let shape = assert_float_const shape in
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= gamma(shape=%f, scale=%f)" assign_var shape
              scale
        | Distr1 (DLaplace, scale) ->
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= laplace(loc=0, scale=%f)" assign_var
              scale (* Assuming loc=0 if not specified *)
        | Distr1 (DCauchy, scale) ->
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= cauchy(loc=0, scale=%f)" assign_var
              scale (* Assuming loc=0 *)
        | Distr1 (DTDist, nu) ->
            let nu = assert_float_const nu in
            Printf.sprintf "%s ~= t(df=%f)" assign_var nu
        | Distr1 (DChi2, nu) ->
            let nu = assert_float_const nu in
            Printf.sprintf "%s ~= chi2(df=%f)" assign_var nu
        | Distr1 (DLogistic, scale) ->
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= logistic(loc=0, scale=%f)" assign_var
              scale (* Assuming loc=0 *)
        | Distr1 (DRayleigh, sigma) ->
            let sigma = assert_float_const sigma in
            Printf.sprintf "%s ~= rayleigh(scale=%f)" assign_var sigma
        | Distr2 (DWeibull, a, b) ->
            let a_val = assert_float_const a in
            let b_val = assert_float_const b in
            Printf.sprintf "%s ~= weibull(c=%f, scale=%f)" assign_var a_val
              b_val (* Assuming a is shape c, b is scale *)
        | Distr2 (DPareto, xm, alpha) ->
            let xm_val = assert_float_const xm in
            let alpha_val = assert_float_const alpha in
            Printf.sprintf "%s ~= pareto(b=%f, scale=%f)" assign_var alpha_val
              xm_val (* SPPL uses b for shape *)
        | Distr2 (DGumbel1, mu, beta_param) ->
            let mu_val = assert_float_const mu in
            let beta_val = assert_float_const beta_param in
            Printf.sprintf "%s ~= gumbel_r(loc=%f, scale=%f)" assign_var mu_val
              beta_val (* gumbel_r for Type I max *)
        | Distr2 (DGumbel2, mu, beta_param) ->
            let mu_val = assert_float_const mu in
            let beta_val = assert_float_const beta_param in
            Printf.sprintf "%s ~= gumbel_l(loc=%f, scale=%f)" assign_var mu_val
              beta_val (* gumbel_l for Type I min *)
        | Distr2 (DExppow, arg1, arg2) ->
            let val1 = assert_float_const arg1 in
            let val2 = assert_float_const arg2 in
            Printf.sprintf "%s ~= exponpow(b=%f, scale=%f)" assign_var val1
              val2 (* Assuming arg1=shape_b, arg2=scale *)
      in
      ([ stmt ], assign_var)
  | ExprNode (DistrCase cases) -> (* Types.ExprNode -> ExprNode *)
      let assign_var =
        match target_var with Some name -> name | None -> fresh_sppl_var state
      in
      (* Translate sub-expressions first (target=None for them) *)
      let prereq_stmts, dict_items =
        List.fold_left_map
          (fun acc (sub_expr, prob) ->
            let sub_stmts, sub_res_expr =
              translate_to_sppl env ~target_var:None sub_expr state
            in
            let key_str =
              match sub_expr with
              | ExprNode (Const _) -> sub_res_expr (* Types.ExprNode -> ExprNode *)
              | ExprNode (BoolConst _) -> sub_res_expr (* Types.ExprNode -> ExprNode *)
              | _ ->
                  failwith
                    "DistrCase expects constant expressions for SPPL choice \
                     keys (in pretty.ml)"
            in
            (acc @ sub_stmts, Printf.sprintf "%s: %f" key_str prob))
          [] cases
      in
      let choice_dict = "{" ^ String.concat ", " dict_items ^ "}" in
      let sample_stmt =
        Printf.sprintf "%s ~= choice(%s)" assign_var choice_dict
      in
      (prereq_stmts @ [ sample_stmt ], assign_var)
  (* Expression Cases: Assign only if target_var is Some *)
  | ExprNode (Less (e1, e2)) -> ( (* Types.ExprNode -> ExprNode *)
      let stmts1, res1 = translate_to_sppl env ~target_var:None e1 state in
      let stmts2, res2 = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s < %s)" res1 res2 in
      match target_var with
      | Some name ->
          (stmts1 @ stmts2 @ [ Printf.sprintf "%s = %s" name expr_str ], name)
      | None -> (stmts1 @ stmts2, expr_str))
  | ExprNode (LessEq (e1, e2)) -> ( (* Types.ExprNode -> ExprNode *)
      let stmts1, res1 = translate_to_sppl env ~target_var:None e1 state in
      let stmts2, res2 = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s <= %s)" res1 res2 in
      match target_var with
      | Some name ->
          (stmts1 @ stmts2 @ [ Printf.sprintf "%s = %s" name expr_str ], name)
      | None -> (stmts1 @ stmts2, expr_str))
  | ExprNode (And (e1, e2)) -> ( (* Types.ExprNode -> ExprNode *)
      let stmts1, res1 = translate_to_sppl env ~target_var:None e1 state in
      let stmts2, res2 = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s) & (%s)" res1 res2 in
      match target_var with
      | Some name ->
          (stmts1 @ stmts2 @ [ Printf.sprintf "%s = %s" name expr_str ], name)
      | None -> (stmts1 @ stmts2, expr_str))
  | ExprNode (Or (e1, e2)) -> ( (* Types.ExprNode -> ExprNode *)
      let stmts1, res1 = translate_to_sppl env ~target_var:None e1 state in
      let stmts2, res2 = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s) | (%s)" res1 res2 in
      match target_var with
      | Some name ->
          (stmts1 @ stmts2 @ [ Printf.sprintf "%s = %s" name expr_str ], name)
      | None -> (stmts1 @ stmts2, expr_str))
  | ExprNode (Not e) -> ( (* Types.ExprNode -> ExprNode *)
      let stmts1, res1 = translate_to_sppl env ~target_var:None e state in
      let expr_str = Printf.sprintf "not (%s)" res1 in
      match target_var with
      | Some name -> (stmts1 @ [ Printf.sprintf "%s = %s" name expr_str ], name)
      | None -> (stmts1, expr_str))
  (* Let Case: Optimize variable assignment *)
  | ExprNode (Let (x, e1, e2)) -> (* Types.ExprNode -> ExprNode *)
      let stmts1, res1_expr = translate_to_sppl env ~target_var:None e1 state in
      let final_stmts1, x_var_name =
        if is_simple_var res1_expr then (stmts1, res1_expr)
          (* Use the existing variable directly *)
        else
          (* Assign complex expression or constant to a temp var *)
          let tmp_x = fresh_sppl_var state in
          (stmts1 @ [ Printf.sprintf "%s = %s" tmp_x res1_expr ], tmp_x)
      in
      let new_env = (x, x_var_name) :: env in
      (* Pass the original target_var down to the body *)
      let stmts2, res2_expr = translate_to_sppl new_env ~target_var e2 state in
      (final_stmts1 @ stmts2, res2_expr)
  (* If Case: Result must be assigned - Attempting target propagation *)
  | ExprNode (If (cond_e, then_e, else_e)) -> (* Types.ExprNode -> ExprNode *)
      let cond_stmts, cond_expr =
        translate_to_sppl env ~target_var:None cond_e state
      in
      let final_res_var =
        match target_var with Some name -> name | None -> fresh_sppl_var state
      in
      (* Translate branches, forcing result into final_res_var *)
      let then_stmts, _ =
        translate_to_sppl env ~target_var:(Some final_res_var) then_e state
      in
      let else_stmts, _ =
        translate_to_sppl env ~target_var:(Some final_res_var) else_e state
      in

      (* Indent statements generated *by the branches* *)
      let indent s = "    " ^ s in
      let full_then_block = List.map indent then_stmts in
      let full_else_block = List.map indent else_stmts in

      let final_stmts =
        cond_stmts
        @ [ Printf.sprintf "if %s:" cond_expr ]
        @ full_then_block @ [ "else:" ] @ full_else_block
      in
      (final_stmts, final_res_var)
  (* Result is always the value in final_res_var *)
  (* Observe Case: Handle Observe for SPPL *)
  | ExprNode (Observe e) -> (* Types.ExprNode -> ExprNode *)
      let cond_stmts, cond_expr =
        translate_to_sppl env ~target_var:None e state
      in
      let observe_stmt = Printf.sprintf "condition(%s)" cond_expr in
      (cond_stmts @ [ observe_stmt ], "")
      (* Observe returns unit, effectively no result name for SPPL value
         assignment *)
  | ExprNode (Fix (f, x, _)) -> (* Types.ExprNode -> ExprNode *)
      (* SPPL does not support recursive functions *)
      failwith
        (Printf.sprintf
           "Recursive functions (fix %s %s := ...) are not supported in SPPL \
            translation."
           f x)
  (* Fail on unsupported features *)
  | ExprNode (Pair _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (First _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (Second _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (Fun _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (FuncApp _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (LoopApp _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (FinConst _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (FinLt _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (FinLeq _) -> (* Types.ExprNode -> ExprNode *)
      let err_msg =
        Printf.sprintf
          "Encountered an unsupported expression type (%s) during SPPL \
           translation (in pretty.ml)."
          (match expr with
          | ExprNode (Pair _) -> "Pair" (* Types.ExprNode -> ExprNode *)
          | ExprNode (First _) -> "First" (* Types.ExprNode -> ExprNode *)
          | ExprNode (Second _) -> "Second" (* Types.ExprNode -> ExprNode *)
          | ExprNode (Fun _) -> "Fun" (* Types.ExprNode -> ExprNode *)
          | ExprNode (FuncApp _) -> "FuncApp" (* Types.ExprNode -> ExprNode *)
          | ExprNode (LoopApp _) -> "LoopApp" (* Types.ExprNode -> ExprNode *)
          | ExprNode (FinConst _) -> "FinConst" (* Types.ExprNode -> ExprNode *)
          | ExprNode (FinLt _) -> "FinLt" (* Types.ExprNode -> ExprNode *)
          | ExprNode (FinLeq _) -> "FinLeq" (* Types.ExprNode -> ExprNode *)
          | _ -> "Other Unsupported")
      in
      failwith err_msg
  | ExprNode Nil | ExprNode (Cons _) | ExprNode (MatchList _) (* Types.ExprNode -> ExprNode *)
    ->
      failwith
        "List constructs (nil, ::, match) are not supported in SPPL \
         translation."
  | ExprNode (Ref _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (Deref _) (* Types.ExprNode -> ExprNode *)
  | ExprNode (Assign (_, _)) -> (* Types.ExprNode -> ExprNode *)
      failwith "References (ref, !, :=) are not supported in SPPL translation."
  | ExprNode (Seq (_, _)) -> (* Types.ExprNode -> ExprNode *)
      (* This is the correct place *)
      failwith "Sequences (e1; e2) are not supported in SPPL translation."
  | ExprNode Unit -> ([], "None") (* Types.ExprNode -> ExprNode *)
  | ExprNode (FinEq (e1, e2, n)) -> (* Types.ExprNode -> ExprNode *)
      failwith
        (Printf.sprintf
           "FinEq (%s ==#%d %s) is not directly supported in SPPL translation."
           (string_of_expr_indented e1)
           n
           (string_of_expr_indented e2))
  | ExprNode (RuntimeError s) -> (* Types.ExprNode -> ExprNode *)
      let err_msg =
        Printf.sprintf "RUNTIME_ERROR(\"%s%s%s\")" variable_color s reset_color
      in
      ([ err_msg ], "")

(**
 * @function cdice_expr_to_sppl_prog
 * @brief Top-level function to convert a ContDice expression into a complete SPPL program string.
 *
 * Initializes SPPL state and calls `translate_to_sppl`. The result of the
 * main ContDice expression is typically assigned to a SPPL variable named "model".
 *
 * @param expr The ContDice expression (`Ast.expr`) to convert.
 * @return A string containing the generated SPPL program.
 *)
let cdice_expr_to_sppl_prog (expr : expr) : string =
  let state = { next_var = 0 } in (* Initialize SPPL variable generation state *)
  (* Translate the main expression, targeting the SPPL variable "model" for the final result. *)
  let sppl_statements, _final_result_variable_name =
    translate_to_sppl [] ~target_var:(Some "model") expr state
  in
  String.concat "\n" sppl_statements (* Join all generated SPPL statements into a single string. *)
