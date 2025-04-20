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
and string_of_expr_node ?(indent=0) (ExprNode expr_node : expr) : string =
  match expr_node with
  | Const f -> 
      Printf.sprintf "%s%g%s" number_color f reset_color
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, e1, e2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_expr_indented ~indent:(indent+2) e1 in
      let e2_str = string_of_expr_indented ~indent:(indent+2) e2 in
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
  | LessEq (e, f) ->
      Printf.sprintf "%s %s<=%s %s%g%s"
        (string_of_expr_indented ~indent e) operator_color reset_color number_color f reset_color
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
      Printf.sprintf "%s(%s%s, %s%s)%s" paren_color e1_str reset_color e2_str reset_color paren_color
  | First e ->
      let e_str = string_of_expr_indented ~indent e in
      Printf.sprintf "%s(%sfst%s %s)%s" paren_color keyword_color reset_color e_str paren_color
  | Second e ->
      let e_str = string_of_expr_indented ~indent e in
      Printf.sprintf "%s(%ssnd%s %s)%s" paren_color keyword_color reset_color e_str paren_color
  | Fun (x, e) ->
      let e_str = string_of_expr_indented ~indent:(indent+2) e in
      Printf.sprintf "%sfun%s %s%s%s %s->%s %s"
        keyword_color reset_color variable_color x reset_color
        operator_color reset_color e_str
  | App (e1, e2) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s(%s%s %s%s)%s" paren_color e1_str reset_color e2_str reset_color paren_color
  | FinConst (k, n) ->
      Printf.sprintf "%s%d%s#%s%d%s" number_color k reset_color operator_color n reset_color
  | FinLt (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s<#%s%d%s %s"
        e1_str operator_color number_color n reset_color e2_str
  | FinLeq (e1, e2, n) ->
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent e2 in
      Printf.sprintf "%s %s<=#%s%d%s %s"
        e1_str operator_color number_color n reset_color e2_str

(* Pretty printer for aexpr nodes *)
and string_of_aexpr_node ?(indent=0) (TAExprNode ae_node : aexpr) : string =
 match ae_node with
  | Const f -> 
      Printf.sprintf "%s%g%s" number_color f reset_color
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, te1, te2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_texpr_indented ~indent:(indent+2) te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) te2 in
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
  | LessEq (te, f) ->
      Printf.sprintf "%s %s<=%s %s%g%s"
        (string_of_texpr_indented ~indent te) operator_color reset_color number_color f reset_color
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
      Printf.sprintf "%s(%s%s, %s%s)%s" paren_color e1_str reset_color e2_str reset_color paren_color
  | First te ->
      let e_str = string_of_texpr_indented ~indent te in
      Printf.sprintf "%s(%sfst%s %s)%s" paren_color keyword_color reset_color e_str paren_color
  | Second te ->
      let e_str = string_of_texpr_indented ~indent te in
      Printf.sprintf "%s(%ssnd%s %s)%s" paren_color keyword_color reset_color e_str paren_color
  | Fun (x, te) ->
      let e_str = string_of_texpr_indented ~indent:(indent+2) te in
      Printf.sprintf "%sfun%s %s%s%s %s->%s %s"
        keyword_color reset_color variable_color x reset_color
        operator_color reset_color e_str
  | App (te1, te2) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s(%s%s %s%s)%s" paren_color e1_str reset_color e2_str reset_color paren_color
  | FinConst (k, n) ->
      Printf.sprintf "%s%d%s#%s%d%s" number_color k reset_color operator_color n reset_color
  | FinLt (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s<#%s%d%s %s"
        e1_str operator_color number_color n reset_color e2_str
  | FinLeq (te1, te2, n) ->
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent te2 in
      Printf.sprintf "%s %s<=#%s%d%s %s"
        e1_str operator_color number_color n reset_color e2_str

(* Pretty printer for types *)
and string_of_ty ty = 
  (* Helper to avoid infinite loops with recursive meta types *)
  let memo = ref [] in
  let rec aux t =
    match Types.force t with (* Use Types.force instead of Contdice.force *)
    | TBool -> Printf.sprintf "%sbool%s" type_color reset_color
    | TFloat (bound_bag_ref, const_bag_ref) ->
        (* Get the bag contents and format them *)
        let bounds_str = 
          match BoundBag.get bound_bag_ref with (* Match on the result of get *)
          | Top -> "T"
          | Finite bound_set ->
              if BoundSet.is_empty bound_set then ""
              else
                let string_of_bound = function 
                  | Bags.Less c -> Printf.sprintf "<%g" c 
                  | Bags.LessEq c -> Printf.sprintf "<=%g" c 
                in
                let elements = BoundSet.elements bound_set in
                String.concat "," (List.map (fun b -> Printf.sprintf "%s%s%s" number_color (string_of_bound b) reset_color) elements)
        in
        let consts_str = 
          match FloatBag.get const_bag_ref with (* Match on the result of get *)
          | Top -> "T"
          | Finite float_set ->
              if FloatSet.is_empty float_set then ""
              else 
                let elements = FloatSet.elements float_set in
                String.concat "," (List.map (fun f -> Printf.sprintf "%s%g%s" number_color f reset_color) elements)
        in
        let content_str = 
          match bounds_str, consts_str with
          | "", "" -> ""
          | b, "" -> Printf.sprintf "%s[%s]%s" bracket_color b reset_color
          | "", c -> Printf.sprintf "%s[; %s]%s" bracket_color c reset_color
          | b, c  -> Printf.sprintf "%s[%s; %s]%s" bracket_color b c reset_color
        in
        Printf.sprintf "%sfloat%s%s" type_color reset_color content_str
    | TPair (t1, t2) ->
          Printf.sprintf "%s(%s * %s)%s" 
            bracket_color (aux t1) (aux t2) reset_color
    | TFun (t1, t2) ->
          Printf.sprintf "%s(%s -> %s)%s" 
            bracket_color (aux t1) (aux t2) reset_color
    | TFin n -> 
        Printf.sprintf "%s#%s%d%s" type_color operator_color n reset_color (* Format #n *)
    | TMeta r ->
        if List.exists (fun r' -> r == r') !memo then
          "..." (* Break cycle *)
        else
          match !r with
          | Some t' -> memo := r :: !memo; let s = aux t' in memo := List.tl !memo; s
          | None -> "?"
  in aux ty

(* Wrappers *)
let string_of_expr expr =
  string_of_expr_indented expr

let string_of_texpr texpr =
  string_of_texpr_indented texpr

let string_of_aexpr aexpr =
  string_of_aexpr_indented aexpr 

(* ======== Debug Printers (OCaml Syntax) ======== *)

(* Placeholder debug string functions for Bags (replace if Bags provides them) *)
let bound_bag_debug_string (_ : Bags.BoundBag.bag) = "<BoundBag.bag>" (* Simple placeholder *)
let float_bag_debug_string (_ : Bags.FloatBag.bag) = "<FloatBag.bag>" (* Simple placeholder *)

(* Module to hold debug printers for bags to avoid naming conflicts *) 
module DebugBagPrinters = struct
  module BoundBag = struct
    include Bags.BoundBag
    let debug_string = bound_bag_debug_string
  end
  module FloatBag = struct
    include Bags.FloatBag
    let debug_string = float_bag_debug_string
  end
end

(* Mutually recursive debug printers *) 
let rec debug_string_of_ty ty = 
  match Types.force ty with (* Use Types.force to resolve meta vars *)
  | TBool -> "TBool"
  | TFloat (b_ref, c_ref) ->
      Printf.sprintf "TFloat(%s, %s)"
        (DebugBagPrinters.BoundBag.debug_string b_ref) 
        (DebugBagPrinters.FloatBag.debug_string c_ref) 
  | TPair (t1, t2) ->
      Printf.sprintf "TPair(%s, %s)" (debug_string_of_ty t1) (debug_string_of_ty t2)
  | TFun (t1, t2) ->
      Printf.sprintf "TFun(%s, %s)" (debug_string_of_ty t1) (debug_string_of_ty t2)
  | TFin n -> Printf.sprintf "TFin(%d)" n
  | TMeta r ->
      (match !r with
       | None -> Printf.sprintf "TMeta(ref None)"
       (* Avoid recursion for potentially cyclic types in debug output *)
       | Some _ -> Printf.sprintf "TMeta(ref Some(...))" )

and debug_string_of_expr (ExprNode node) =
  Printf.sprintf "ExprNode(%s)" (debug_string_of_expr_node node)

and debug_string_of_expr_node (node : expr expr_generic) = 
  match node with
  | Const f -> Printf.sprintf "Const(%g)" f
  | Var x -> Printf.sprintf "Var(\"%s\")" x
  | Let (x, e1, e2) -> Printf.sprintf "Let(\"%s\", %s, %s)" x (debug_string_of_expr e1) (debug_string_of_expr e2)
  | CDistr _ -> Printf.sprintf "CDistr(%s)" "<cdistr>" (* Assume Stats.string_of_cdistr exists or use placeholder *)
  | Discrete ps -> Printf.sprintf "Discrete([%s])" (String.concat "; " (List.map string_of_float ps))
  | Less (e, f) -> Printf.sprintf "Less(%s, %g)" (debug_string_of_expr e) f
  | LessEq (e, f) -> Printf.sprintf "LessEq(%s, %g)" (debug_string_of_expr e) f
  | If (e1, e2, e3) -> Printf.sprintf "If(%s, %s, %s)" (debug_string_of_expr e1) (debug_string_of_expr e2) (debug_string_of_expr e3)
  | Pair (e1, e2) -> Printf.sprintf "Pair(%s, %s)" (debug_string_of_expr e1) (debug_string_of_expr e2)
  | First e -> Printf.sprintf "First(%s)" (debug_string_of_expr e)
  | Second e -> Printf.sprintf "Second(%s)" (debug_string_of_expr e)
  | Fun (x, e) -> Printf.sprintf "Fun(\"%s\", %s)" x (debug_string_of_expr e)
  | App (e1, e2) -> Printf.sprintf "App(%s, %s)" (debug_string_of_expr e1) (debug_string_of_expr e2)
  | FinConst (k, n) -> Printf.sprintf "FinConst(%d, %d)" k n
  | FinLt (e1, e2, n) -> Printf.sprintf "FinLt(%s, %s, %d)" (debug_string_of_expr e1) (debug_string_of_expr e2) n
  | FinLeq (e1, e2, n) -> Printf.sprintf "FinLeq(%s, %s, %d)" (debug_string_of_expr e1) (debug_string_of_expr e2) n

and debug_string_of_texpr ((ty, aexpr) : texpr) =
  Printf.sprintf "(%s, %s)" (debug_string_of_ty ty) (debug_string_of_aexpr aexpr)

and debug_string_of_aexpr (TAExprNode node) =
   Printf.sprintf "TAExprNode(%s)" (debug_string_of_aexpr_node node)

and debug_string_of_aexpr_node (node : texpr expr_generic) = 
   match node with
  | Const f -> Printf.sprintf "Const(%g)" f
  | Var x -> Printf.sprintf "Var(\"%s\")" x
  | Let (x, te1, te2) -> Printf.sprintf "Let(\"%s\", %s, %s)" x (debug_string_of_texpr te1) (debug_string_of_texpr te2)
  | CDistr _ -> Printf.sprintf "CDistr(%s)" "<cdistr>"
  | Discrete ps -> Printf.sprintf "Discrete([%s])" (String.concat "; " (List.map string_of_float ps))
  | Less (te, f) -> Printf.sprintf "Less(%s, %g)" (debug_string_of_texpr te) f
  | LessEq (te, f) -> Printf.sprintf "LessEq(%s, %g)" (debug_string_of_texpr te) f
  | If (te1, te2, te3) -> Printf.sprintf "If(%s, %s, %s)" (debug_string_of_texpr te1) (debug_string_of_texpr te2) (debug_string_of_texpr te3)
  | Pair (te1, te2) -> Printf.sprintf "Pair(%s, %s)" (debug_string_of_texpr te1) (debug_string_of_texpr te2)
  | First te -> Printf.sprintf "First(%s)" (debug_string_of_texpr te)
  | Second te -> Printf.sprintf "Second(%s)" (debug_string_of_texpr te)
  | Fun (x, te) -> Printf.sprintf "Fun(\"%s\", %s)" x (debug_string_of_texpr te)
  | App (te1, te2) -> Printf.sprintf "App(%s, %s)" (debug_string_of_texpr te1) (debug_string_of_texpr te2)
  | FinConst (k, n) -> Printf.sprintf "FinConst(%d, %d)" k n
  | FinLt (te1, te2, n) -> Printf.sprintf "FinLt(%s, %s, %d)" (debug_string_of_texpr te1) (debug_string_of_texpr te2) n
  | FinLeq (te1, te2, n) -> Printf.sprintf "FinLeq(%s, %s, %d)" (debug_string_of_texpr te1) (debug_string_of_texpr te2) n 