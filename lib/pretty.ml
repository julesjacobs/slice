open Types
open Bag

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
| Stats.Uniform (lo, hi) -> 
    Printf.sprintf "%suniform%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color lo reset_color number_color hi reset_color
| Stats.Gaussian (mean, std) -> 
    Printf.sprintf "%sgaussian%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color mean reset_color number_color std reset_color
| Stats.Exponential rate -> 
    Printf.sprintf "%sexponential%s(%s%g%s)" 
        keyword_color reset_color number_color rate reset_color
| Stats.Beta (alpha, beta) -> 
    Printf.sprintf "%sbeta%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color alpha reset_color number_color beta reset_color

(* Forward declarations *)
let rec string_of_expr_indented ?(indent=0) e =
  string_of_expr_node ~indent e
and string_of_aexpr_indented ?(indent=0) ae =
  string_of_aexpr_node ~indent ae
and string_of_texpr_indented ?(indent=0) ((ty, aexpr) : texpr) : string =
  let aexpr_str = string_of_aexpr_indented ~indent aexpr in
  Printf.sprintf "%s(%s%s : %s%s)%s"
    paren_color reset_color aexpr_str (string_of_ty ty) paren_color reset_color

(* Pretty printer for expr nodes *)
and string_of_expr_node ?(indent=0) (ExprNode expr_node) : string =
  match expr_node with
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, e1, e2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_expr_indented ~indent:(indent+2) e1 in (* Recurse with expr *)
      let e2_str = string_of_expr_indented ~indent:(indent+2) e2 in (* Recurse with expr *)
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s"
        keyword_color reset_color variable_color x reset_color e1_str
        keyword_color reset_color indent_str e2_str
  | CDistr dist -> string_of_cdistr dist
  | Discrete probs ->
      Printf.sprintf "%sdiscrete%s(%s%s%s)"
        keyword_color reset_color number_color
        (String.concat ", " (List.map (fun f -> Printf.sprintf "%g" f) probs))
        reset_color
  | Less (e, f) ->
      Printf.sprintf "%s %s<%s %s%g%s"
        (string_of_expr_indented ~indent e) operator_color reset_color number_color f reset_color
  | LessEq (e, n) ->
      Printf.sprintf "%s %s<=%s %s%d%s"
        (string_of_expr_indented ~indent e) operator_color reset_color number_color n reset_color
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
  | App (e1, e2) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "(%s %s)" e1_str e2_str

(* Pretty printer for aexpr nodes *)
and string_of_aexpr_node ?(indent=0) (TAExprNode ae_node) : string =
 match ae_node with
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, te1, te2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_texpr_indented ~indent:(indent+2) te1 in (* Recurse with texpr *)
      let e2_str = string_of_texpr_indented ~indent:(indent+2) te2 in (* Recurse with texpr *)
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s"
        keyword_color reset_color variable_color x reset_color e1_str
        keyword_color reset_color indent_str e2_str
  | CDistr dist -> string_of_cdistr dist
  | Discrete probs ->
      Printf.sprintf "%sdiscrete%s(%s%s%s)"
        keyword_color reset_color number_color
        (String.concat ", " (List.map (fun f -> Printf.sprintf "%g" f) probs))
        reset_color
  | Less (te, f) ->
      Printf.sprintf "%s %s<%s %s%g%s"
        (string_of_texpr_indented ~indent te) operator_color reset_color number_color f reset_color
  | LessEq (te, n) ->
      Printf.sprintf "%s %s<=%s %s%d%s"
        (string_of_texpr_indented ~indent te) operator_color reset_color number_color n reset_color
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
  | App (te1, te2) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "(%s %s)" e1_str e2_str

(* Pretty printer for types - Needs to be part of the recursive block *)
and string_of_ty = function
  | TBool -> Printf.sprintf "%sbool%s" type_color reset_color
  | TInt -> Printf.sprintf "%sint%s" type_color reset_color
  | TFloat bag ->
      let root = Bag.find bag in (* Use Bag.find *)
      (match !root with
      | Root { elems } ->
          if FloatSet.is_empty elems then
            Printf.sprintf "%sfloat%s" type_color reset_color
          else
            let elements = FloatSet.elements elems in
            let str_elems = String.concat ", " 
              (List.map (fun f -> Printf.sprintf "%s%g%s" number_color f reset_color) elements) in
            Printf.sprintf "%sfloat%s%s<%s%s>%s" 
              type_color reset_color bracket_color str_elems bracket_color reset_color
      | Link _ -> failwith "Impossible: find returned a Link")
    | TPair (t1, t2) ->
        Printf.sprintf "%s(%s * %s)%s" 
          bracket_color (string_of_ty t1) (string_of_ty t2) reset_color
    | TFun (t1, t2) ->
        Printf.sprintf "%s(%s -> %s)%s" 
          bracket_color (string_of_ty t1) (string_of_ty t2) reset_color
    | TMeta r ->
        match !r with
        | Some t -> string_of_ty t
        | None -> "?"

(* Wrappers *)
let string_of_expr expr =
  string_of_expr_indented expr

let string_of_texpr texpr =
  string_of_texpr_indented texpr

let string_of_aexpr aexpr =
  string_of_aexpr_indented aexpr 