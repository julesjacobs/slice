(*
This module provides functions for pretty-printing various constructs of the
language, including expressions (`expr`), typed expressions (`texpr`),
annotated expressions (`aexpr`), types (`ty`), and continuous distributions (`cdistr`).
It uses ANSI color codes for syntax highlighting when printing to a terminal.
Additionally, this module includes functionality to translate expressions from
this language into the SPPL (Simple Probabilistic Programming Language) format.
*)
open Types
open Bags (* Open Bags to get FloatSet and access Set modules and Bound type *)

(* ANSI color codes for syntax highlighting in terminal output. *)
let keyword_color = "\027[1;34m"  (* Bold Blue *)
let operator_color = "\027[1;31m" (* Bold Red *)
let number_color = "\027[0;32m"   (* Green *)
let variable_color = "\027[0;33m" (* Yellow *)
let reset_color = "\027[0m"       (* Reset to default terminal color *)
let paren_color = "\027[1;37m"   (* Bold White, used for parentheses *)
let type_color = "\027[1;35m"    (* Bold Magenta, used for type annotations *)
let bracket_color = "\027[1;36m" (* Bold Cyan, used for brackets in types *)

(** [string_of_cdistr dist] converts a continuous distribution from `Distributions.cdistr`
    type to its string representation, including keyword highlighting.
    If a distribution is added to `Distributions.cdistr` but not handled here,
    it will fall into the wildcard case. *)
let string_of_cdistr = function
| Distributions.Uniform (lo, hi) -> 
    Printf.sprintf "%suniform%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color lo reset_color number_color hi reset_color
| Distributions.Gaussian (mean, std) -> 
    Printf.sprintf "%sgaussian%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color mean reset_color number_color std reset_color
| Distributions.Exponential rate -> 
    Printf.sprintf "%sexponential%s(%s%g%s)" 
        keyword_color reset_color number_color rate reset_color
| Distributions.Beta (alpha, beta) -> 
    Printf.sprintf "%sbeta%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color alpha reset_color number_color beta reset_color
| Distributions.LogNormal (mu, sigma) ->
    Printf.sprintf "%slognormal%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color mu reset_color number_color sigma reset_color
| Distributions.Gamma (shape, scale) ->
    Printf.sprintf "%sgamma%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color shape reset_color number_color scale reset_color
| Distributions.Laplace scale ->
    Printf.sprintf "%slaplace%s(%s%g%s)" 
        keyword_color reset_color number_color scale reset_color
| Distributions.Cauchy scale ->
    Printf.sprintf "%scauchy%s(%s%g%s)" 
        keyword_color reset_color number_color scale reset_color
| Distributions.Weibull (a, b) ->
    Printf.sprintf "%sweibull%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color a reset_color number_color b reset_color
| Distributions.TDist nu ->
    Printf.sprintf "%stdist%s(%s%g%s)" 
        keyword_color reset_color number_color nu reset_color
| Distributions.Chi2 nu ->
    Printf.sprintf "%schi2%s(%s%g%s)" 
        keyword_color reset_color number_color nu reset_color
| Distributions.Logistic scale ->
    Printf.sprintf "%slogistic%s(%s%g%s)" 
        keyword_color reset_color number_color scale reset_color
(* The wildcard case handles any distributions not explicitly listed above.
   This might indicate an incomplete match if new distributions are added to Distributions.cdistr. *)
| _ -> "<unsupported distribution>"

(* Forward declarations for mutually recursive pretty-printing functions.
   These allow `string_of_expr_indented` to call `string_of_aexpr_indented` (via `string_of_texpr_indented`),
   and `string_of_aexpr_indented` to call `string_of_texpr_indented` for sub-expressions.
   Indentation level is passed around to format nested structures. *)
let rec string_of_expr_indented ?(indent=0) e =
  string_of_expr_node ~indent e
and string_of_aexpr_indented ?(indent=0) ae =
  string_of_aexpr_node ~indent ae
(** [string_of_texpr_indented texpr] pretty-prints a typed expression (`texpr`),
    which is a pair of a type (`ty`) and an annotated expression node (`aexpr_node`).
    It includes the type annotation in the output string, e.g., `(expression : type)`.
    For `FinConst` nodes, it omits the redundant type annotation if it's just `k#n`. *)
and string_of_texpr_indented ?(indent=0) ((ty, aexpr) : texpr) : string =
  let aexpr_str = string_of_aexpr_indented ~indent aexpr in
  match aexpr with
  | TAExprNode (FinConst (_, _)) -> aexpr_str (* For FinConst, just print the constant itself, e.g., 3#5 *)
  | _ -> 
      (* Default: print expression with its inferred type annotation *)
      Printf.sprintf "%s(%s%s : %s%s)%s"
        paren_color reset_color aexpr_str (string_of_ty ty) paren_color reset_color

(* [string_of_expr_node expr_node] pretty-prints an untyped expression node (`expr_node`).
   It handles various language constructs with appropriate formatting and color coding. *)
and string_of_expr_node ?(indent=0) (ExprNode expr_node) : string =
  match expr_node with
  | Const f -> 
      Printf.sprintf "%s%g%s" number_color f reset_color
  | BoolConst b -> 
      Printf.sprintf "%s%b%s" keyword_color b reset_color
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, e1, e2) -> (* Formats 'let x = e1 in e2' with indentation *)
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_expr_indented ~indent:(indent+2) e1 in
      let e2_str = string_of_expr_indented ~indent:(indent+2) e2 in
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s"
        keyword_color reset_color variable_color x reset_color e1_str
        keyword_color reset_color indent_str e2_str
  | Sample dist_exp -> string_of_sample ~indent dist_exp
  | DistrCase cases -> (* Formats 'discrete(p1: e1, p2: e2, ...)' *)
      let format_case (expr, prob) =
        Printf.sprintf "%s%g%s: %s"
          number_color prob reset_color (string_of_expr_indented ~indent expr)
      in
      Printf.sprintf "%sdiscrete%s(%s%s%s)"
        keyword_color reset_color paren_color
        (String.concat ", " (List.map format_case cases))
        reset_color
  | Cmp (cmp_op, e1, e2, flipped) -> (* Handles comparisons, flips back > to < if needed for display *)
      let op_str, left_expr, right_expr = 
        if flipped then (* If originally e.g. e1 > e2, it's stored as Cmp(Lt, e2, e1, true). Print as e1 > e2. *)
          match cmp_op with
          | Types.Lt -> ">", e2, e1
          | Types.Le -> ">=", e2, e1
        else
          match cmp_op with
          | Types.Lt -> "<", e1, e2
          | Types.Le -> "<=", e1, e2
      in
      Printf.sprintf "%s %s%s%s %s"
        (string_of_expr_indented ~indent left_expr) operator_color op_str reset_color (string_of_expr_indented ~indent right_expr)
  | FinCmp (cmp_op, e1, e2, n, flipped) -> (* Similar to Cmp but for finite domain comparisons like e1 <#n e2 *)
      let op_str, left_expr, right_expr = 
        if flipped then
          match cmp_op with
          | Types.Lt -> ">#", e2, e1
          | Types.Le -> ">=#", e2, e1
        else
          match cmp_op with
          | Types.Lt -> "<#", e1, e2
          | Types.Le -> "<=#", e1, e2
      in
      Printf.sprintf "%s %s%s%s%s%d%s %s"
        (string_of_expr_indented ~indent left_expr) operator_color op_str reset_color type_color n reset_color (string_of_expr_indented ~indent right_expr)
  | Not e1 ->
      Printf.sprintf "(%snot%s %s)"
        operator_color reset_color (string_of_expr_indented ~indent e1)
  | And (e1, e2) ->
      Printf.sprintf "%s %s&&%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | Or (e1, e2) ->
      Printf.sprintf "%s %s||%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | If (e1, e2, e3) -> (* Formats 'if e1 then e2 else e3' with indentation *)
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent+2) ' ' in
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent:(indent+2) e2 in
      let e3_str = string_of_expr_indented ~indent:(indent+2) e3 in
      Printf.sprintf "%sif%s %s %sthen%s\n%s%s\n%s%selse%s\n%s%s"
        keyword_color reset_color e1_str keyword_color reset_color
        next_indent_str e2_str indent_str keyword_color reset_color next_indent_str e3_str
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
      let e_str = string_of_expr_indented ~indent:(indent+2) e in
      Printf.sprintf "%sfun%s %s%s%s %s->%s %s"
        keyword_color reset_color variable_color x reset_color
        operator_color reset_color e_str
  | FuncApp (e1, e2) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "(%s %s)" e1_str e2_str
  | LoopApp (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "(%s %s %d)" e1_str e2_str n
  | Fix (f, x, e) ->
      let e_str = string_of_expr_indented ~indent:(indent+2) e in
      Printf.sprintf "%sfix%s %s%s%s %s%s%s %s:=%s %s"
        keyword_color reset_color variable_color f reset_color variable_color x reset_color
        operator_color reset_color e_str
  | FinConst (k, n) ->
      Printf.sprintf "%s%d%s%s#%d%s" number_color k reset_color type_color n reset_color
  | FinEq (e1, e2, n) ->
      Printf.sprintf "%s %s==%s%s#%d%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color type_color n reset_color (string_of_expr_indented ~indent e2)
  | Observe e1 ->
      let e1_str = string_of_expr_indented ~indent e1 in
      Printf.sprintf "%sobserve%s (%s)"
        keyword_color reset_color e1_str
  | Nil -> Printf.sprintf "%snil%s" keyword_color reset_color
  | Cons (e1, e2) -> 
      Printf.sprintf "%s %s::%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | MatchList (e1, e_nil, y, ys, e_cons) -> (* Formats 'match e1 with | nil -> e_nil | y::ys -> e_cons end' *)
      let e1_str = string_of_expr_indented ~indent:(indent+2) e1 in
      let e_nil_str = string_of_expr_indented ~indent:(indent+2) e_nil in
      let e_cons_str = string_of_expr_indented ~indent:(indent+4) e_cons in
      Printf.sprintf "%smatch%s %s %swith%s\n%s  | %snil%s %s->%s %s\n%s  | %s%s%s %s::%s %s%s%s %s->%s %s\n%s%send%s"
        keyword_color reset_color e1_str keyword_color reset_color
        (String.make indent ' ') keyword_color reset_color operator_color reset_color e_nil_str
        (String.make indent ' ') variable_color y reset_color operator_color reset_color variable_color ys reset_color operator_color reset_color e_cons_str
        (String.make indent ' ') keyword_color reset_color
  | Ref e1 -> 
      Printf.sprintf "%sref%s %s" 
        keyword_color reset_color (string_of_expr_indented ~indent e1)
  | Deref e1 -> 
      Printf.sprintf "%s!%s%s" 
        operator_color reset_color (string_of_expr_indented ~indent e1)
  | Assign (e1, e2) -> 
      Printf.sprintf "%s %s:=%s %s" 
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | Seq (e1, e2) -> 
      Printf.sprintf "%s %s;%s %s" 
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | Unit -> Printf.sprintf "%s()%s" keyword_color reset_color
  | RuntimeError s -> Printf.sprintf "%sRUNTIME_ERROR%s(\"%s%s%s\")" operator_color reset_color variable_color s reset_color

(* Pretty printer for distribution expressions within Sample nodes (expr context) *)
and string_of_sample ?(indent=0) dist_exp = 
  match dist_exp with
  | Distr1 (kind, e1) -> 
      Printf.sprintf "%s%s%s(%s)" 
        keyword_color (Distributions.string_of_single_arg_dist_kind kind) reset_color (string_of_expr_indented ~indent e1)
  | Distr2 (kind, e1, e2) -> 
      Printf.sprintf "%s%s%s(%s, %s)" 
        keyword_color (Distributions.string_of_two_arg_dist_kind kind) reset_color (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)

(* [string_of_aexpr_node aexpr_node] pretty-prints an annotated expression node (`aexpr_node`).
   The logic largely mirrors `string_of_expr_node` but operates on `texpr` for sub-expressions. *)
and string_of_aexpr_node ?(indent=0) (TAExprNode ae_node) : string =
 match ae_node with
  | Const f -> 
      Printf.sprintf "%s%g%s" number_color f reset_color
  | BoolConst b -> 
      Printf.sprintf "%s%b%s" keyword_color b reset_color
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, te1, te2) -> (* Formats 'let x = (e1:t1) in (e2:t2)' *)
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_texpr_indented ~indent:(indent+2) te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) te2 in
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s"
        keyword_color reset_color variable_color x reset_color e1_str
        keyword_color reset_color indent_str e2_str
  | Sample dist_exp -> string_of_asample ~indent dist_exp
  | DistrCase cases -> (* Formats 'discrete(p1: (e1:t1), p2: (e2:t2), ...)' *)
      let format_case (texpr, prob) =
        Printf.sprintf "%s%g%s: %s"
          number_color prob reset_color (string_of_texpr_indented ~indent texpr)
      in
      Printf.sprintf "%sdiscrete%s(%s%s%s)"
        keyword_color reset_color paren_color
        (String.concat ", " (List.map format_case cases))
        reset_color
  | Cmp (cmp_op, te1, te2, flipped) -> (* Handles comparisons, e.g. (e1:t1) < (e2:t2) *)
      let op_str, left_expr, right_expr = 
        if flipped then
          match cmp_op with
          | Types.Lt -> ">", te2, te1
          | Types.Le -> ">=", te2, te1
        else
          match cmp_op with
          | Types.Lt -> "<", te1, te2
          | Types.Le -> "<=", te1, te2
      in
      Printf.sprintf "%s %s%s%s %s"
        (string_of_texpr_indented ~indent left_expr) operator_color op_str reset_color (string_of_texpr_indented ~indent right_expr)
  | FinCmp (cmp_op, te1, te2, n, flipped) ->
      let op_str, left_expr, right_expr = 
        if flipped then
          match cmp_op with
          | Types.Lt -> ">#", te2, te1
          | Types.Le -> ">=#", te2, te1
        else
          match cmp_op with
          | Types.Lt -> "<#", te1, te2
          | Types.Le -> "<=#", te1, te2
      in
      Printf.sprintf "%s %s%s%s%s%d%s %s"
        (string_of_texpr_indented ~indent left_expr) operator_color op_str reset_color type_color n reset_color (string_of_texpr_indented ~indent right_expr)
  | Not te1 ->
      Printf.sprintf "(%snot%s %s)"
        operator_color reset_color (string_of_texpr_indented ~indent te1)
  | And (te1, te2) ->
      Printf.sprintf "%s %s&&%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | Or (te1, te2) ->
      Printf.sprintf "%s %s||%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | If (te1, te2, te3) ->
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent+2) ' ' in
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) te2 in
      let e3_str = string_of_texpr_indented ~indent:(indent+2) te3 in
      Printf.sprintf "%sif%s %s %sthen%s\n%s%s\n%s%selse%s\n%s%s"
        keyword_color reset_color e1_str keyword_color reset_color
        next_indent_str e2_str indent_str keyword_color reset_color next_indent_str e3_str
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
      let e_str = string_of_texpr_indented ~indent:(indent+2) te in
      Printf.sprintf "%sfun%s %s%s%s %s->%s %s"
        keyword_color reset_color variable_color x reset_color
        operator_color reset_color e_str
  | FuncApp (te1, te2) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "(%s %s)" e1_str e2_str
  | LoopApp (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "(%s %s %d)" e1_str e2_str n
  | Fix (f, x, te) ->
      let te_str = string_of_texpr_indented ~indent:(indent+2) te in
      Printf.sprintf "%sfix%s %s%s%s %s%s%s %s:=%s %s"
        keyword_color reset_color variable_color f reset_color variable_color x reset_color
        operator_color reset_color te_str
  | FinConst (k, n) ->
      Printf.sprintf "%s%d%s%s#%d%s" number_color k reset_color type_color n reset_color
  | FinEq (te1, te2, n) ->
      Printf.sprintf "%s %s==%s%s#%d%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color type_color n reset_color (string_of_texpr_indented ~indent te2)
  | Observe te1 ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      Printf.sprintf "%sobserve%s (%s)"
        keyword_color reset_color e1_str
  | Nil -> Printf.sprintf "%snil%s" keyword_color reset_color
  | Cons (te1, te2) -> 
      Printf.sprintf "%s %s::%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | MatchList (te1, te_nil, y, ys, te_cons) ->
      let te1_str = string_of_texpr_indented ~indent:(indent+2) te1 in
      let te_nil_str = string_of_texpr_indented ~indent:(indent+2) te_nil in
      let te_cons_str = string_of_texpr_indented ~indent:(indent+4) te_cons in
      Printf.sprintf "%smatch%s %s %swith%s\n%s  | %snil%s %s->%s %s\n%s  | %s%s%s %s::%s %s%s%s %s->%s %s\n%s%send%s"
        keyword_color reset_color te1_str keyword_color reset_color
        (String.make indent ' ') keyword_color reset_color operator_color reset_color te_nil_str
        (String.make indent ' ') variable_color y reset_color operator_color reset_color variable_color ys reset_color operator_color reset_color te_cons_str
        (String.make indent ' ') keyword_color reset_color
  | Ref te1 -> 
      Printf.sprintf "%sref%s %s" 
        keyword_color reset_color (string_of_texpr_indented ~indent te1)
  | Deref te1 -> 
      Printf.sprintf "%s!%s%s" 
        operator_color reset_color (string_of_texpr_indented ~indent te1)
  | Assign (te1, te2) -> 
      Printf.sprintf "%s %s:=%s %s" 
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | Seq (te1, te2) -> 
      Printf.sprintf "%s %s;%s %s" 
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | Unit -> Printf.sprintf "%s()%s" keyword_color reset_color
  | RuntimeError s -> Printf.sprintf "%sRUNTIME_ERROR%s(\"%s%s%s\")" operator_color reset_color variable_color s reset_color

(* Pretty printer for types *)
and string_of_ty = function
  | TBool -> Printf.sprintf "%sbool%s" type_color reset_color
  | TFloat (bound_bag_ref, const_bag_ref) ->
      (* Formats TFloat as `float[bounds_info; consts_info]` *)
      let bounds_str = 
        match BoundBag.get bound_bag_ref with
        | Top -> "T" (* Bounds are Top *)
        | Finite bound_set ->
            if BoundSet.is_empty bound_set then "" (* No specific bounds *)
            else
              let string_of_bound = function (* Helper to format individual bounds *)
                | Bags.Less c -> Printf.sprintf "<%g" c 
                | Bags.LessEq c -> Printf.sprintf "<=%g" c 
                | Bags.Greater c -> Printf.sprintf ">%g" c 
                | Bags.GreaterEq c -> Printf.sprintf ">=%g" c 
              in
              let elements = BoundSet.elements bound_set in
              String.concat "," (List.map string_of_bound elements)
      in
      let consts_str = 
        match FloatBag.get const_bag_ref with
        | Top -> "T" (* Constants are Top (unknown) *)
        | Finite float_set ->
            if FloatSet.is_empty float_set then "" (* No specific known constants *)
            else 
              let elements = FloatSet.elements float_set in
              String.concat "," (List.map (Printf.sprintf "%g") elements)
      in
      let content_str = (* Constructs the content string for brackets, e.g., "[bounds; consts]" *)
        match bounds_str, consts_str with
        | "", "" -> "" (* No bounds or consts info, just "float" *)
        | b, "" -> Printf.sprintf "%s[%s]%s" type_color b reset_color 
        | "", c -> Printf.sprintf "%s[; %s]%s" type_color c reset_color (* Leading semicolon if only consts *)
        | b, c  -> Printf.sprintf "%s[%s; %s]%s" type_color b c reset_color
      in
      Printf.sprintf "%sfloat%s%s" type_color reset_color content_str
  | TPair (t1, t2) ->
        Printf.sprintf "%s(%s * %s)%s" 
          bracket_color (string_of_ty t1) (string_of_ty t2) reset_color
  | TFun (t1, t2) ->
        Printf.sprintf "%s(%s -> %s)%s" 
          bracket_color (string_of_ty t1) (string_of_ty t2) reset_color
  | TFin n -> 
        Printf.sprintf "%s#%d%s" type_color n reset_color (* e.g., #5 for TFin 5 *)
  | TUnit -> Printf.sprintf "%sunit%s" type_color reset_color
  | TList t -> Printf.sprintf "%slist%s %s" type_color reset_color (string_of_ty t)
  | TRef t -> Printf.sprintf "%s%s ref%s" type_color (string_of_ty t) reset_color
  | TMeta r -> (* Prints meta variables, showing Known type or '?' for Unknown *)
        match !r with
        | Known t -> string_of_ty t (* If resolved, print the known type *)
        | Unknown _ -> "?" (* If unresolved, print '?' *)
(* Pretty printer for distribution expressions within Sample nodes (texpr/aexpr context) *)
and string_of_asample ?(indent=0) dist_exp =
  match dist_exp with
  | Distr1 (kind, te1) -> 
      Printf.sprintf "%s%s%s(%s)" 
        keyword_color (Distributions.string_of_single_arg_dist_kind kind) reset_color (string_of_texpr_indented ~indent te1)
  | Distr2 (kind, te1, te2) -> 
      Printf.sprintf "%s%s%s(%s, %s)" 
        keyword_color (Distributions.string_of_two_arg_dist_kind kind) reset_color (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)

(* Wrappers for external use, defaulting indentation to 0. *)
let string_of_expr expr =
  string_of_expr_indented expr

let string_of_texpr texpr =
  string_of_texpr_indented texpr

let string_of_aexpr aexpr =
  string_of_aexpr_indented aexpr 

let string_of_float_list (l : float list) : string =
  "[" ^ (String.concat "; " (List.map (Printf.sprintf "%g") l)) ^ "]"

(* ===================================================== *)
(* SPPL Conversion Logic (Integrated into Pretty module) *)
(* ===================================================== *)

(* State for generating unique variable names for SPPL.
   `next_var` is a mutable counter to ensure freshness. *)
type sppl_state = {
  mutable next_var : int;
}

(** [fresh_sppl_var state] generates a new unique variable name (e.g., "x1", "x2")
    by incrementing the counter in `state`. *)
let fresh_sppl_var state =
  state.next_var <- state.next_var + 1;
  Printf.sprintf "x%d" state.next_var

(** [is_simple_var s] checks if a string `s` already represents a variable
    generated by `fresh_sppl_var` (e.g., "x12"). This is used in `Let` translation
    to avoid re-assigning an already simple variable. *)
let is_simple_var s =
  String.length s > 0 && s.[0] = 'x' &&
  try ignore (int_of_string (String.sub s 1 (String.length s - 1))); true
  with Failure _ -> false

(** [translate_to_sppl env target_var expr state] recursively translates an expression
    `expr` into a list of SPPL statements and the name of the SPPL variable or
    constant holding the result.
    - `env`: Maps original variable names to their SPPL counterparts.
    - `target_var`: An optional string. If `Some name`, the function tries to assign
      the result of the current expression directly to `name`. This can optimize
      SPPL code by reducing temporary variables (e.g. in `Let` or `If` assignments).
      If `None`, a fresh variable might be generated if the expression isn't a simple value.
    - `expr`: The expression to translate.
    - `state`: The `sppl_state` for generating fresh variable names.
    Returns a pair: `(sppl_statements_list, result_sppl_variable_or_value_string)`.
*)
let rec translate_to_sppl (env : (string * string) list) ?(target_var:string option=None) (expr : Types.expr) (state : sppl_state) : (string list * string) =
  match expr with
  (* Base Cases: Constants and Variables *)
  | Types.ExprNode(Const f) ->
      let val_str = string_of_float f in
      (match target_var with
       | Some name -> ([Printf.sprintf "%s = %s" name val_str], name) (* Assign to target if provided *)
       | None -> ([], val_str)) (* Otherwise, return the value string directly *)
  | Types.ExprNode(BoolConst b) ->
      let val_str = string_of_bool b |> String.capitalize_ascii in (* SPPL uses "True", "False" *)
      (match target_var with
       | Some name -> ([Printf.sprintf "%s = %s" name val_str], name)
       | None -> ([], val_str))
  | Types.ExprNode(Var x) ->
      let var_name = 
        try List.assoc x env (* Look up original var name in SPPL env *)
        with Not_found -> failwith ("Unbound variable during SPPL translation: " ^ x)
      in
      (match target_var with
        | Some name when name <> var_name -> ([Printf.sprintf "%s = %s" name var_name], name) (* Assign to target if different *)
        | Some name (* when name = var_name *) -> ([], name) (* Target is already the correct variable *)
        | None -> ([], var_name)) (* No target, just use the looked-up SPPL variable name *)

  (* Sampling Cases: These must be assigned to a variable in SPPL. *)
  | Types.ExprNode(Sample d) ->
      let assign_var = match target_var with Some name -> name | None -> fresh_sppl_var state in
      (* `assert_float_const` ensures distribution parameters are constants, as SPPL often requires this.
         This might be a limitation if the target SPPL dialect supports variable parameters. *)
      let assert_float_const e = 
        match e with
        | Types.ExprNode(Const f) -> f
        | _ -> failwith "Expected a constant expression for SPPL translation because SPPL does not support non-constant expressions in sampling (in pretty.ml)"
      in
      let stmt = match d with
        | Distr2 (DUniform, a, b) ->
            let a = assert_float_const a in
            let b = assert_float_const b in
            Printf.sprintf "%s ~= uniform(loc=%f, scale=%f)" assign_var a (b -. a) (* SPPL uniform: loc=low, scale=high-low *)
        | Distr2 (DGaussian, mu, sigma) ->
            let mu = assert_float_const mu in
            let sigma = assert_float_const sigma in
            Printf.sprintf "%s ~= normal(loc=%f, scale=%f)" assign_var mu sigma
        | Distr1 (DExponential, rate) -> 
            let rate = assert_float_const rate in
            Printf.sprintf "%s ~= exponential(scale=%f)" assign_var (1.0 /. rate) (* SPPL exponential uses scale = 1/rate (mean) *)
        | Distr2 (DBeta, alpha, beta_param) -> 
            let alpha = assert_float_const alpha in
            let beta_val = assert_float_const beta_param in
            Printf.sprintf "%s ~= beta(a=%f, b=%f)" assign_var alpha beta_val
        | Distr2 (DLogNormal, mu, sigma) -> 
            let mu = assert_float_const mu in
            let sigma = assert_float_const sigma in
            Printf.sprintf "%s ~= lognormal(mu=%f, sigma=%f)" assign_var mu sigma (* SPPL lognormal often mu=meanlog, sigma=sdlog *)
        | Distr2 (DGamma, shape, scale) ->
            let shape = assert_float_const shape in
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= gamma(shape=%f, scale=%f)" assign_var shape scale
        | Distr1 (DLaplace, scale) ->
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= laplace(loc=0, scale=%f)" assign_var scale (* Assuming loc=0 if not specified in source *)
        | Distr1 (DCauchy, scale) ->
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= cauchy(loc=0, scale=%f)" assign_var scale (* Assuming loc=0 *)
        | Distr1 (DTDist, nu) ->
            let nu = assert_float_const nu in
            Printf.sprintf "%s ~= t(df=%f)" assign_var nu
        | Distr1 (DChi2, nu) ->
            let nu = assert_float_const nu in
            Printf.sprintf "%s ~= chi2(df=%f)" assign_var nu
        | Distr1 (DLogistic, scale) ->
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= logistic(loc=0, scale=%f)" assign_var scale (* Assuming loc=0 *)
        | Distr1 (DRayleigh, sigma) ->
            let sigma = assert_float_const sigma in
            Printf.sprintf "%s ~= rayleigh(scale=%f)" assign_var sigma
        | Distr2 (DWeibull, a, b) ->
            let a_val = assert_float_const a in (* shape/concentration `c` in SPPL *)
            let b_val = assert_float_const b in (* scale in SPPL *)
            Printf.sprintf "%s ~= weibull(c=%f, scale=%f)" assign_var a_val b_val
        | Distr2 (DPareto, xm, alpha) ->
            let xm_val = assert_float_const xm in (* scale in SPPL *)
            let alpha_val = assert_float_const alpha in (* shape `b` in SPPL *)
            Printf.sprintf "%s ~= pareto(b=%f, scale=%f)" assign_var alpha_val xm_val
        | Distr2 (DGumbel1, mu, beta_param) -> (* Gumbel Type I (maxima) *)
            let mu_val = assert_float_const mu in
            let beta_val = assert_float_const beta_param in
            Printf.sprintf "%s ~= gumbel_r(loc=%f, scale=%f)" assign_var mu_val beta_val
        | Distr2 (DGumbel2, mu, beta_param) -> (* Gumbel Type II (minima) - often gumbel_l in SPPL context *)
            let mu_val = assert_float_const mu in (* This is 'a' (shape) for gumbel_l in some SPPLs, or loc for others *)
            let beta_val = assert_float_const beta_param in (* This is 'scale' for gumbel_l *)
            Printf.sprintf "%s ~= gumbel_l(loc=%f, scale=%f)" assign_var mu_val beta_val (* Assuming loc/scale for gumbel_l. May need adjustment based on target SPPL variant. *)
        | Distr2 (DExppow, arg1, arg2) -> (* Exponential Power distribution *)
            let val1 = assert_float_const arg1 in (* shape `b` in SPPL `exponpow` *)
            let val2 = assert_float_const arg2 in (* scale in SPPL `exponpow` - this is unusual, often loc/scale/power *)
            Printf.sprintf "%s ~= exponpow(b=%f, scale=%f)" assign_var val1 val2 (* Check SPPL specific parameters *)
      in
      ([stmt], assign_var)
  | Types.ExprNode(DistrCase cases) -> (* Translates to SPPL 'choice' *)
      let assign_var = match target_var with Some name -> name | None -> fresh_sppl_var state in
      (* Sub-expressions for choice outcomes must be constants in many SPPL versions. *)
      let (prereq_stmts, dict_items) =
        List.fold_left_map (fun acc (sub_expr, prob) ->
          let (sub_stmts, sub_res_expr) = translate_to_sppl env ~target_var:None sub_expr state in
          let key_str =
            match sub_expr with (* Ensure choice keys are simple values *)
            | Types.ExprNode(Const _) -> sub_res_expr
            | Types.ExprNode(BoolConst _) -> sub_res_expr 
            | _ -> failwith "DistrCase expects constant expressions for SPPL choice keys (in pretty.ml)"
          in
          (acc @ sub_stmts, Printf.sprintf "%s: %f" key_str prob)
        ) [] cases
      in
      let choice_dict = "{" ^ (String.concat ", " dict_items) ^ "}" in
      let sample_stmt = Printf.sprintf "%s ~= choice(%s)" assign_var choice_dict in
      (prereq_stmts @ [sample_stmt], assign_var)

  (* Standard Expression Cases: Result assigned to target_var or used directly. *)
  | Types.ExprNode(Cmp (cmp_op, e1, e2, flipped)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let op_str, left_res, right_res = 
        if flipped then (* Original was e.g. e1 > e2, stored as Cmp(Lt, e2, e1, true) *)
          match cmp_op with Types.Lt -> ">", res2, res1 | Types.Le -> ">=", res2, res1
        else
          match cmp_op with Types.Lt -> "<", res1, res2 | Types.Le -> "<=", res1, res2
      in
      let expr_str = Printf.sprintf "(%s %s %s)" left_res op_str right_res in
      (match target_var with 
       | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1 @ stmts2, expr_str))
  | Types.ExprNode(FinCmp (cmp_op, e1, e2, n, flipped)) ->
      (* SPPL: Finite domain comparisons (FinCmp) are not directly supported. *)
      let op_str_src = match cmp_op, flipped with
        | Types.Lt, false -> "<#" | Types.Le, false -> "<=#"
        | Types.Lt, true  -> ">#" | Types.Le, true  -> ">=#"
      in
      failwith (Printf.sprintf "FinCmp (%s %s%d %s) is not directly supported in SPPL translation." 
        (string_of_expr_indented e1) op_str_src n (string_of_expr_indented e2))
  | Types.ExprNode(Not e) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e state in
      let expr_str = Printf.sprintf "not (%s)" res1 in
      (match target_var with
       | Some name -> (stmts1 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1, expr_str))
  | Types.ExprNode(And (e1, e2)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s and %s)" res1 res2 in
      (match target_var with
       | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1 @ stmts2, expr_str))
  | Types.ExprNode(Or (e1, e2)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s or %s)" res1 res2 in
      (match target_var with
       | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1 @ stmts2, expr_str))

  (* Let Case: Introduce new variable, optimize if e1's result is already a simple var. *)
  | Types.ExprNode(Let(x, e1, e2)) ->
      let (stmts1, res1_expr) = translate_to_sppl env ~target_var:None e1 state in
      let (final_stmts1, x_sppl_name) =
        if is_simple_var res1_expr then (* If e1 resulted in a simple var (e.g. xN), use it directly *)
          (stmts1, res1_expr)
        else (* Otherwise, assign e1's result to a fresh SPPL variable for x *)
          let tmp_x = fresh_sppl_var state in
          (stmts1 @ [Printf.sprintf "%s = %s" tmp_x res1_expr], tmp_x)
      in
      let new_env = (x, x_sppl_name) :: env in
      (* Translate body e2, potentially assigning its result to the original target_var *)
      let (stmts2, res2_expr) = translate_to_sppl new_env ~target_var:target_var e2 state in 
      (final_stmts1 @ stmts2, res2_expr)

  (* If Case: Result is assigned to final_res_var (target_var or fresh).
     Branches are translated to assign to this same final_res_var. *)
  | Types.ExprNode(If(cond_e, then_e, else_e)) ->
      let (cond_stmts, cond_expr) = translate_to_sppl env ~target_var:None cond_e state in
      let final_res_var = match target_var with Some name -> name | None -> fresh_sppl_var state in
      (* Translate branches, forcing their results into `final_res_var` *)
      let (then_stmts, _) = translate_to_sppl env ~target_var:(Some final_res_var) then_e state in
      let (else_stmts, _) = translate_to_sppl env ~target_var:(Some final_res_var) else_e state in
      
      let indent s = "    " ^ s in (* Helper for indenting SPPL blocks *)
      let full_then_block = List.map indent then_stmts in
      let full_else_block = List.map indent else_stmts in
      
      let final_stmts =
        cond_stmts @
        [Printf.sprintf "if %s:" cond_expr] @
        full_then_block @
        ["else:"] @
        full_else_block
      in
      (final_stmts, final_res_var) (* Result is always the value in final_res_var *)

  (* Observe Case: Handle Observe for SPPL *)
  | Types.ExprNode(Observe e) ->
      let (cond_stmts, cond_expr) = translate_to_sppl env ~target_var:None e state in
      let observe_stmt = Printf.sprintf "condition(%s)" cond_expr in
      (cond_stmts @ [observe_stmt], "") (* Observe returns unit, effectively no result name for SPPL value assignment *)

  | Types.ExprNode(Fix (f,x,_)) -> (* SPPL does not support recursive functions *)
      failwith (Printf.sprintf "Recursive functions (fix %s %s := ...) are not supported in SPPL translation." f x)

  (* Fail on unsupported features *) 
  | Types.ExprNode(Pair _) | Types.ExprNode(First _) | Types.ExprNode(Second _)
  | Types.ExprNode(Fun _) | Types.ExprNode(FuncApp _) | Types.ExprNode(LoopApp _)
  | Types.ExprNode(FinConst _) ->
      let err_msg = Printf.sprintf
        "Encountered an unsupported expression type (%s) during SPPL translation (in pretty.ml)."
        (match expr with
         | Types.ExprNode(Pair _) -> "Pair" | Types.ExprNode(First _) -> "First" | Types.ExprNode(Second _) -> "Second"
         | Types.ExprNode(Fun _) -> "Fun" | Types.ExprNode(FuncApp _) -> "FuncApp" | Types.ExprNode(LoopApp _) -> "LoopApp" 
         | Types.ExprNode(FinConst _) -> "FinConst"
         | _ -> "Other Unsupported")
      in
      failwith err_msg

  | Types.ExprNode(Nil) | Types.ExprNode(Cons _) | Types.ExprNode(MatchList _) ->
      failwith "List constructs (nil, ::, match) are not supported in SPPL translation."

  | Types.ExprNode(Ref _) | Types.ExprNode(Deref _) | Types.ExprNode(Assign (_,_)) ->
      failwith "References (ref, !, :=) are not supported in SPPL translation."

  | Types.ExprNode(Seq (_,_)) -> (* This is the correct place *) 
      failwith "Sequences (e1; e2) are not supported in SPPL translation."

  | Types.ExprNode(Unit) ->
      ([], "None")

  | Types.ExprNode(FinEq (e1, e2, n)) ->
      failwith (Printf.sprintf "FinEq (%s ==#%d %s) is not directly supported in SPPL translation." 
        (string_of_expr_indented e1) n (string_of_expr_indented e2))

  | Types.ExprNode(RuntimeError s) ->
      let err_msg = Printf.sprintf "RUNTIME_ERROR(\"%s%s%s\")" variable_color s reset_color in
      ([err_msg], "")

(* Top-level function: call translate with target 'model' *) 
let cdice_expr_to_sppl_prog (expr : Types.expr) : string =
  let state = { next_var = 0 } in
  (* Pass target_var = Some "model" to the top-level call *) 
  let (stmts, _final_res_name) = translate_to_sppl [] ~target_var:(Some "model") expr state in
  (* No need for the extra assignment at the end now *) 
  String.concat "\n" stmts 