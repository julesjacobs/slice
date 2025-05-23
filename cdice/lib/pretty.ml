open Types
open Bags (* Open Bags to get FloatSet and access Set modules and Bound type *)

(* ANSI color codes for syntax highlighting *)
let keyword_color = "\027[1;34m"  (* Bold Blue *)
let operator_color = "\027[1;31m" (* Bold Red *)
let number_color = "\027[0;32m"   (* Green *)
let variable_color = "\027[0;33m" (* Yellow *)
let reset_color = "\027[0m"       (* Reset *)
let paren_color = "\027[1;37m"   (* Bold White *)
let type_color = "\027[1;35m"    (* Bold Magenta *)
let bracket_color = "\027[1;36m" (* Bold Cyan *)

(* Pretty printer for continuous distributions *)
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
| _ -> "<unsupported distribution>"

(* Forward declarations *)
let rec string_of_expr_indented ?(indent=0) e =
  string_of_expr_node ~indent e
and string_of_aexpr_indented ?(indent=0) ae =
  string_of_aexpr_node ~indent ae
and string_of_texpr_indented ?(indent=0) ((ty, aexpr) : texpr) : string =
  let aexpr_str = string_of_aexpr_indented ~indent aexpr in
  (* Check if the expression is FinConst *)
  match aexpr with
  | TAExprNode (FinConst (_, _)) -> aexpr_str (* Just print the constant *)
  | _ -> 
      (* Default behavior: print expression with type *)
      Printf.sprintf "%s(%s%s : %s%s)%s"
        paren_color reset_color aexpr_str (string_of_ty ty) paren_color reset_color

(* Pretty printer for expr nodes *)
and string_of_expr_node ?(indent=0) (ExprNode expr_node) : string =
  match expr_node with
  | Const f -> 
      Printf.sprintf "%s%g%s" number_color f reset_color
  | BoolConst b -> 
      Printf.sprintf "%s%b%s" keyword_color b reset_color
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, e1, e2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_expr_indented ~indent:(indent+2) e1 in
      let e2_str = string_of_expr_indented ~indent:(indent+2) e2 in
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s"
        keyword_color reset_color variable_color x reset_color e1_str
        keyword_color reset_color indent_str e2_str
  | Sample dist_exp -> string_of_sample ~indent dist_exp
  | DistrCase cases ->
      let format_case (expr, prob) =
        Printf.sprintf "%s%g%s: %s"
          number_color prob reset_color (string_of_expr_indented ~indent expr)
      in
      Printf.sprintf "%sdiscrete%s(%s%s%s)"
        keyword_color reset_color paren_color
        (String.concat ", " (List.map format_case cases))
        reset_color
  | Less (e1, e2) ->
      Printf.sprintf "%s %s<%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | LessEq (e1, e2) ->
      Printf.sprintf "%s %s<=%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | Greater (e1, e2) ->
      Printf.sprintf "%s %s>%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | GreaterEq (e1, e2) ->
      Printf.sprintf "%s %s>=%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | And (e1, e2) ->
      Printf.sprintf "%s %s&&%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | Or (e1, e2) ->
      Printf.sprintf "%s %s||%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | Not e1 ->
      Printf.sprintf "(%snot%s %s)"
        operator_color reset_color (string_of_expr_indented ~indent e1)
  | If (e1, e2, e3) ->
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
  | FinLt (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s<%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | FinLeq (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s<=%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | FinGt (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s>%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | FinGeq (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s>=%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | FinEq (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s==#%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | Observe e1 ->
      let e1_str = string_of_expr_indented ~indent e1 in
      Printf.sprintf "%sobserve%s (%s)"
        keyword_color reset_color e1_str
  | Nil -> Printf.sprintf "%snil%s" keyword_color reset_color
  | Cons (e1, e2) -> 
      Printf.sprintf "%s %s::%s %s"
        (string_of_expr_indented ~indent e1) operator_color reset_color (string_of_expr_indented ~indent e2)
  | MatchList (e1, e_nil, y, ys, e_cons) ->
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

and string_of_sample ?(indent=0) dist_exp = 
  match dist_exp with
  | Distr1 (kind, e1) -> 
      Printf.sprintf "%s%s%s(%s)" 
        keyword_color (Distributions.string_of_single_arg_dist_kind kind) reset_color (string_of_expr_indented ~indent e1)
  | Distr2 (kind, e1, e2) -> 
      Printf.sprintf "%s%s%s(%s, %s)" 
        keyword_color (Distributions.string_of_two_arg_dist_kind kind) reset_color (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)

(* Pretty printer for aexpr nodes *)
and string_of_aexpr_node ?(indent=0) (TAExprNode ae_node) : string =
 match ae_node with
  | Const f -> 
      Printf.sprintf "%s%g%s" number_color f reset_color
  | BoolConst b -> 
      Printf.sprintf "%s%b%s" keyword_color b reset_color
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, te1, te2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_texpr_indented ~indent:(indent+2) te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) te2 in
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s"
        keyword_color reset_color variable_color x reset_color e1_str
        keyword_color reset_color indent_str e2_str
  | Sample dist_exp -> string_of_asample ~indent dist_exp
  | DistrCase cases ->
      let format_case (texpr, prob) =
        Printf.sprintf "%s%g%s: %s"
          number_color prob reset_color (string_of_texpr_indented ~indent texpr)
      in
      Printf.sprintf "%sdiscrete%s(%s%s%s)"
        keyword_color reset_color paren_color
        (String.concat ", " (List.map format_case cases))
        reset_color
  | Less (te1, te2) ->
      Printf.sprintf "%s %s<%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | LessEq (te1, te2) ->
      Printf.sprintf "%s %s<=%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | Greater (te1, te2) ->
      Printf.sprintf "%s %s>%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | GreaterEq (te1, te2) ->
      Printf.sprintf "%s %s>=%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | And (te1, te2) ->
      Printf.sprintf "%s %s&&%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | Or (te1, te2) ->
      Printf.sprintf "%s %s||%s %s"
        (string_of_texpr_indented ~indent te1) operator_color reset_color (string_of_texpr_indented ~indent te2)
  | Not te1 ->
      Printf.sprintf "(%snot%s %s)"
        operator_color reset_color (string_of_texpr_indented ~indent te1)
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
  | FinLt (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s<%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | FinLeq (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s<=%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | FinGt (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s>%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | FinGeq (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s>=%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
  | FinEq (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s==#%s%s#%d%s %s"
        e1_str operator_color reset_color type_color n reset_color e2_str
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
      let bounds_str = 
        match BoundBag.get bound_bag_ref with
        | Top -> "T"
        | Finite bound_set ->
            if BoundSet.is_empty bound_set then ""
            else
              let string_of_bound = function 
                | Bags.Less c -> Printf.sprintf "<%g" c 
                | Bags.LessEq c -> Printf.sprintf "<=%g" c 
                | Bags.Greater c -> Printf.sprintf ">%g" c 
                | Bags.GreaterEq c -> Printf.sprintf ">=%g" c 
              in
              let elements = BoundSet.elements bound_set in
              (* Join with comma, no space. Removed internal colors *)
              String.concat "," (List.map string_of_bound elements)
      in
      let consts_str = 
        match FloatBag.get const_bag_ref with
        | Top -> "T"
        | Finite float_set ->
            if FloatSet.is_empty float_set then ""
            else 
              let elements = FloatSet.elements float_set in
              (* Join with comma, no space. Removed internal colors *)
              String.concat "," (List.map (Printf.sprintf "%g") elements)
      in
      let content_str = 
        match bounds_str, consts_str with
        | "", "" -> "" (* Just float *)
        (* Apply type_color around the whole bracketed content *)
        | b, "" -> Printf.sprintf "%s[%s]%s" type_color b reset_color 
        | "", c -> Printf.sprintf "%s[; %s]%s" type_color c reset_color
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
        Printf.sprintf "%s#%d%s" type_color n reset_color
  | TUnit -> Printf.sprintf "%sunit%s" type_color reset_color
  | TList t -> Printf.sprintf "%slist%s %s" type_color reset_color (string_of_ty t)
  | TRef t -> Printf.sprintf "%s%s ref%s" type_color (string_of_ty t) reset_color
  | TMeta r ->
        match !r with
        | Known t -> string_of_ty t
        | Unknown _ -> "?"
and string_of_asample ?(indent=0) dist_exp =
  match dist_exp with
  | Distr1 (kind, te1) -> 
      Printf.sprintf "%s%s%s(%s)" 
        keyword_color (Distributions.string_of_single_arg_dist_kind kind) reset_color (string_of_texpr_indented ~indent te1)
  | Distr2 (kind, te1, te2) -> 
      Printf.sprintf "%s%s%s(%s, %s)" 
        keyword_color (Distributions.string_of_two_arg_dist_kind kind) reset_color (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)

(* Wrappers *)
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

(* State for generating unique variable names for SPPL *)
type sppl_state = {
  mutable next_var : int;
}

let fresh_sppl_var state =
  state.next_var <- state.next_var + 1;
  Printf.sprintf "x%d" state.next_var

(* Helper to check if a string represents a simple SPPL variable (e.g., "x12") *)
let is_simple_var s =
  String.length s > 0 && s.[0] = 'x' &&
  try ignore (int_of_string (String.sub s 1 (String.length s - 1))); true
  with Failure _ -> false

(* Main translation function with target variable optimization *)
let rec translate_to_sppl (env : (string * string) list) ?(target_var:string option=None) (expr : Types.expr) (state : sppl_state) : (string list * string) =
  match expr with
  (* Base Cases: Assign directly if target_var is Some *) 
  | Types.ExprNode(Const f) ->
      let val_str = string_of_float f in
      (match target_var with
       | Some name -> ([Printf.sprintf "%s = %s" name val_str], name)
       | None -> ([], val_str))
  | Types.ExprNode(BoolConst b) ->
      let val_str = string_of_bool b |> String.capitalize_ascii in
      (match target_var with
       | Some name -> ([Printf.sprintf "%s = %s" name val_str], name)
       | None -> ([], val_str))
  | Types.ExprNode(Var x) ->
      let var_name = 
        try List.assoc x env 
        with Not_found -> failwith ("Unbound variable during SPPL translation: " ^ x)
      in
      (match target_var with
        | Some name when name <> var_name -> ([Printf.sprintf "%s = %s" name var_name], name)
        | Some name (* when name = var_name *) -> ([], name) (* Target is already the right var *)
        | None -> ([], var_name) (* No target, just return the var name *))

  (* Sampling Cases: Must assign to a variable *) 
  | Types.ExprNode(Sample d) ->
      let assign_var = match target_var with Some name -> name | None -> fresh_sppl_var state in
      let assert_float_const e = 
        match e with
        | Types.ExprNode(Const f) -> f
        | _ -> failwith "Expected a constant expression for SPPL translation because SPPL does not support non-constant expressions in sampling (in pretty.ml)"
      in
      let stmt = match d with
        | Distr2 (DUniform, a, b) ->
            let a = assert_float_const a in
            let b = assert_float_const b in
            Printf.sprintf "%s ~= uniform(loc=%f, scale=%f)" assign_var a (b -. a)
        | Distr2 (DGaussian, mu, sigma) ->
            let mu = assert_float_const mu in
            let sigma = assert_float_const sigma in
            Printf.sprintf "%s ~= normal(loc=%f, scale=%f)" assign_var mu sigma
        | Distr1 (DExponential, rate) -> 
            let rate = assert_float_const rate in
            Printf.sprintf "%s ~= exponential(scale=%f)" assign_var (1.0 /. rate) (* SPPL scale = 1/rate *)
        | Distr2 (DBeta, alpha, beta_param) -> 
            let alpha = assert_float_const alpha in
            let beta_val = assert_float_const beta_param in
            Printf.sprintf "%s ~= beta(a=%f, b=%f)" assign_var alpha beta_val
        | Distr2 (DLogNormal, mu, sigma) -> 
            let mu = assert_float_const mu in
            let sigma = assert_float_const sigma in
            Printf.sprintf "%s ~= lognormal(mu=%f, sigma=%f)" assign_var mu sigma
        | Distr2 (DGamma, shape, scale) ->
            let shape = assert_float_const shape in
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= gamma(shape=%f, scale=%f)" assign_var shape scale
        | Distr1 (DLaplace, scale) ->
            let scale = assert_float_const scale in
            Printf.sprintf "%s ~= laplace(loc=0, scale=%f)" assign_var scale (* Assuming loc=0 if not specified *)
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
            let a_val = assert_float_const a in
            let b_val = assert_float_const b in
            Printf.sprintf "%s ~= weibull(c=%f, scale=%f)" assign_var a_val b_val (* Assuming a is shape c, b is scale *)
        | Distr2 (DPareto, xm, alpha) ->
            let xm_val = assert_float_const xm in
            let alpha_val = assert_float_const alpha in
            Printf.sprintf "%s ~= pareto(b=%f, scale=%f)" assign_var alpha_val xm_val (* SPPL uses b for shape *)
        | Distr2 (DGumbel1, mu, beta_param) ->
            let mu_val = assert_float_const mu in
            let beta_val = assert_float_const beta_param in
            Printf.sprintf "%s ~= gumbel_r(loc=%f, scale=%f)" assign_var mu_val beta_val (* gumbel_r for Type I max *)
        | Distr2 (DGumbel2, mu, beta_param) ->
            let mu_val = assert_float_const mu in
            let beta_val = assert_float_const beta_param in
            Printf.sprintf "%s ~= gumbel_l(loc=%f, scale=%f)" assign_var mu_val beta_val (* gumbel_l for Type I min *)
        | Distr2 (DExppow, arg1, arg2) -> 
            let val1 = assert_float_const arg1 in
            let val2 = assert_float_const arg2 in
            Printf.sprintf "%s ~= exponpow(b=%f, scale=%f)" assign_var val1 val2 (* Assuming arg1=shape_b, arg2=scale *)
      in
      ([stmt], assign_var)
  | Types.ExprNode(DistrCase cases) ->
      let assign_var = match target_var with Some name -> name | None -> fresh_sppl_var state in
      (* Translate sub-expressions first (target=None for them) *) 
      let (prereq_stmts, dict_items) =
        List.fold_left_map (fun acc (sub_expr, prob) ->
          let (sub_stmts, sub_res_expr) = translate_to_sppl env ~target_var:None sub_expr state in
          let key_str =
            match sub_expr with
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

  (* Expression Cases: Assign only if target_var is Some *) 
  | Types.ExprNode(Less(e1, e2)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s < %s)" res1 res2 in
      (match target_var with 
       | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1 @ stmts2, expr_str))
  | Types.ExprNode(LessEq(e1, e2)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s <= %s)" res1 res2 in
      (match target_var with
       | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1 @ stmts2, expr_str))
  | Types.ExprNode(Greater(e1, e2)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s > %s)" res1 res2 in
      (match target_var with 
        | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
        | None -> (stmts1 @ stmts2, expr_str))
  | Types.ExprNode(GreaterEq(e1, e2)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s >= %s)" res1 res2 in
      (match target_var with
        | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
        | None -> (stmts1 @ stmts2, expr_str))
  | Types.ExprNode(And(e1, e2)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s) & (%s)" res1 res2 in
      (match target_var with
       | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1 @ stmts2, expr_str))
  | Types.ExprNode(Or(e1, e2)) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e1 state in
      let (stmts2, res2) = translate_to_sppl env ~target_var:None e2 state in
      let expr_str = Printf.sprintf "(%s) | (%s)" res1 res2 in
      (match target_var with
       | Some name -> (stmts1 @ stmts2 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1 @ stmts2, expr_str))
  | Types.ExprNode(Not e) ->
      let (stmts1, res1) = translate_to_sppl env ~target_var:None e state in
      let expr_str = Printf.sprintf "not (%s)" res1 in
      (match target_var with
       | Some name -> (stmts1 @ [Printf.sprintf "%s = %s" name expr_str], name)
       | None -> (stmts1, expr_str))

  (* Let Case: Optimize variable assignment *) 
  | Types.ExprNode(Let(x, e1, e2)) ->
      let (stmts1, res1_expr) = translate_to_sppl env ~target_var:None e1 state in
      let (final_stmts1, x_var_name) =
        if is_simple_var res1_expr then
          (stmts1, res1_expr) (* Use the existing variable directly *)
        else
          (* Assign complex expression or constant to a temp var *) 
          let tmp_x = fresh_sppl_var state in
          (stmts1 @ [Printf.sprintf "%s = %s" tmp_x res1_expr], tmp_x)
      in
      let new_env = (x, x_var_name) :: env in
      (* Pass the original target_var down to the body *) 
      let (stmts2, res2_expr) = translate_to_sppl new_env ~target_var:target_var e2 state in 
      (final_stmts1 @ stmts2, res2_expr)

  (* If Case: Result must be assigned - Attempting target propagation *) 
  | Types.ExprNode(If(cond_e, then_e, else_e)) ->
      let (cond_stmts, cond_expr) = translate_to_sppl env ~target_var:None cond_e state in
      let final_res_var = match target_var with Some name -> name | None -> fresh_sppl_var state in
      (* Translate branches, forcing result into final_res_var *) 
      let (then_stmts, _) = translate_to_sppl env ~target_var:(Some final_res_var) then_e state in
      let (else_stmts, _) = translate_to_sppl env ~target_var:(Some final_res_var) else_e state in
      
      (* Indent statements generated *by the branches* *) 
      let indent s = "    " ^ s in
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
  | Types.ExprNode(FinConst _) | Types.ExprNode(FinLt _) | Types.ExprNode(FinLeq _) 
  | Types.ExprNode(FinGt _) | Types.ExprNode(FinGeq _) ->
      let err_msg = Printf.sprintf
        "Encountered an unsupported expression type (%s) during SPPL translation (in pretty.ml)."
        (match expr with
         | Types.ExprNode(Pair _) -> "Pair" | Types.ExprNode(First _) -> "First" | Types.ExprNode(Second _) -> "Second"
         | Types.ExprNode(Fun _) -> "Fun" | Types.ExprNode(FuncApp _) -> "FuncApp" | Types.ExprNode(LoopApp _) -> "LoopApp" 
         | Types.ExprNode(FinConst _) -> "FinConst" | Types.ExprNode(FinLt _) -> "FinLt" | Types.ExprNode(FinLeq _) -> "FinLeq" 
         | Types.ExprNode(FinGt _) -> "FinGt" | Types.ExprNode(FinGeq _) -> "FinGeq"
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