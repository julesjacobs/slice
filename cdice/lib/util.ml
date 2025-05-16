open Types
open Bags (* Open Bags to get FloatSet and access Set modules and Bound type *)

let var_counter = ref 0
let fresh_var (prefix : string) : string =
  incr var_counter;
  prefix ^ string_of_int !var_counter

let gen_let (base_name_hint : string) (rhs_expr : Types.expr) (body_fn : string -> Types.expr) : Types.expr =
  match rhs_expr with
  | Types.ExprNode (Types.Var existing_var_name) ->
      body_fn existing_var_name (* Use existing variable name directly in the body *)
  | _ ->
      let new_var_name = fresh_var base_name_hint in (* Use the existing fresh_var from Util.ml *)
      Types.ExprNode (Types.Let (new_var_name, rhs_expr, body_fn new_var_name))

(* Eventually refactor this so that all print functions go in one file, with an optional argument to specify pretty print *)

(* Pretty printer for continuous distributions *)
let string_of_cdistr = function
| Distributions.Uniform (lo, hi) -> 
    Printf.sprintf "uniform(%g, %g)" lo hi
| Distributions.Gaussian (mean, std) -> 
    Printf.sprintf "gaussian(%g, %g)" mean std
| Distributions.Exponential rate -> 
    Printf.sprintf "exponential(%g)" rate
| Distributions.Beta (alpha, beta) -> 
    Printf.sprintf "beta(%g, %g)" alpha beta
| Distributions.LogNormal (mu, sigma) -> 
    Printf.sprintf "lognormal(%g, %g)" mu sigma
| _ -> "<unsupported distribution>"

let bit_length n =
  if n < 0 then invalid_arg "bit_length: only non-negative integers allowed"
  else if n = 0 then 1
  else
    let rec aux n acc =
      if n = 0 then acc
      else aux (n lsr 1) (acc + 1)
    in
    aux n 0

(* Forward declarations *)
let rec string_of_expr_indented ?(indent=0) e =
  string_of_expr_node ~indent e
and string_of_aexpr_indented ?(indent=0) ae =
  string_of_aexpr_node ~indent ae
and string_of_texpr_indented ?(indent=0) ((ty, aexpr) : texpr) : string =
  let aexpr_str = string_of_aexpr_indented ~indent aexpr in
  match aexpr with
  | TAExprNode (FinConst (_, _)) -> aexpr_str
  | _ -> Printf.sprintf "(%s : %s)" aexpr_str (string_of_ty ty)

and string_of_expr_node ?(indent=0) (ExprNode expr_node) : string =
  match expr_node with
  | Const f -> Printf.sprintf "%g" f
  | BoolConst b -> string_of_bool b
  | Var x -> x
  | Let (x, e1, e2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_expr_indented ~indent:(indent+2) e1 in
      let e2_str = string_of_expr_indented ~indent:(indent+2) e2 in
      Printf.sprintf "let %s = %s in\n%s%s" x e1_str indent_str e2_str
  | Sample dist_exp -> string_of_sample ~indent dist_exp
  | DistrCase cases ->
    let format_case (_, prob) =
      match prob with
      | 0. -> "0."
      | 1. -> "1."
      | _ -> Printf.sprintf "%g" prob
    in
    Printf.sprintf "discrete(%s)" (String.concat ", " (List.map format_case cases))
  | Less (e1, e2) ->
      Printf.sprintf "%s < %s" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | LessEq (e1, e2) ->
      Printf.sprintf "%s <= %s" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | And (e1, e2) ->
      Printf.sprintf "%s && %s" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Or (e1, e2) ->
      Printf.sprintf "%s || %s" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Not e1 ->
      Printf.sprintf "! %s" (string_of_expr_indented ~indent e1)
  | If (e1, e2, e3) ->
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent+2) ' ' in
      let e1_str = string_of_expr_indented ~indent e1 in
      let e2_str = string_of_expr_indented ~indent:(indent+2) e2 in
      let e3_str = string_of_expr_indented ~indent:(indent+2) e3 in
      Printf.sprintf "if %s then\n%s%s\n%selse\n%s%s" e1_str next_indent_str e2_str indent_str next_indent_str e3_str
  | Pair (e1, e2) -> Printf.sprintf "(%s, %s)" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | First e -> Printf.sprintf "(fst %s)" (string_of_expr_indented ~indent e)
  | Second e -> Printf.sprintf "(snd %s)" (string_of_expr_indented ~indent e)
  | Fun (x, e) ->
      let e_str = string_of_expr_indented ~indent:(indent+2) e in
      Printf.sprintf "fun %s -> %s" x e_str
  | App (e1, e2) -> Printf.sprintf "(%s %s)" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | FinConst (k, n) -> Printf.sprintf "int(%d,%d)" (bit_length (n-1)) k
  | FinLt (e1, e2, _) -> Printf.sprintf "%s < %s" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | FinLeq (e1, e2, _) -> Printf.sprintf "%s <= %s" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Observe e1 -> Printf.sprintf "observe (%s)" (string_of_expr_indented ~indent e1)
  | Fix (f, x, e) -> 
      let e_str = string_of_expr_indented ~indent:(indent+2) e in
      Printf.sprintf "fix %s %s := %s" f x e_str
  | Nil -> "nil"
  | Cons (e1, e2) -> 
      Printf.sprintf "%s :: %s"
        (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | MatchList (e1, e_nil, y, ys, e_cons) ->
      let e1_str = string_of_expr_indented ~indent:(indent+2) e1 in
      let e_nil_str = string_of_expr_indented ~indent:(indent+2) e_nil in
      let e_cons_str = string_of_expr_indented ~indent:(indent+4) e_cons in
      Printf.sprintf "match %s with\n%s  | nil -> %s\n%s  | %s :: %s -> %s\n%send"
        e1_str 
        (String.make indent ' ') e_nil_str
        (String.make indent ' ') y ys e_cons_str
        (String.make indent ' ')
  | Ref e1 -> Printf.sprintf "(ref %s)" (string_of_expr_indented ~indent e1)
  | Deref e1 -> Printf.sprintf "(!%s)" (string_of_expr_indented ~indent e1)
  | Assign (e1, e2) -> Printf.sprintf "(%s := %s)" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Seq (e1, e2) -> Printf.sprintf "%s; %s" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Unit -> "()"
and string_of_sample ?(indent=0) dist_exp = 
  match dist_exp with
  | Distr2 (DUniform, e1, e2) -> Printf.sprintf "uniform(%s, %s)" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Distr2 (DGaussian, e1, e2) -> Printf.sprintf "gaussian(%s, %s)" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Distr1 (DExponential, e1) -> Printf.sprintf "exponential(%s)" (string_of_expr_indented ~indent e1)
  | Distr2 (DBeta, e1, e2) -> Printf.sprintf "beta(%s, %s)" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Distr2 (DLogNormal, e1, e2) -> Printf.sprintf "lognormal(%s, %s)" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | Distr2 (DGamma, e1, e2) -> Printf.sprintf "gamma(%s, %s)" (string_of_expr_indented ~indent e1) (string_of_expr_indented ~indent e2)
  | _ -> "<other distribution>"

and string_of_aexpr_node ?(indent=0) (TAExprNode ae_node) : string =
  match ae_node with
  | Const f -> Printf.sprintf "%g" f
  | BoolConst b -> string_of_bool b
  | Var x -> x
  | Let (x, te1, te2) ->
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_texpr_indented ~indent:(indent+2) te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) te2 in
      Printf.sprintf "let %s = %s in\n%s%s" x e1_str indent_str e2_str
  | Sample dist_exp -> string_of_asample dist_exp 
  | DistrCase cases ->
    let format_case (_, prob) =
      match prob with
      | 0. -> "0."
      | 1. -> "1."
      | _ -> Printf.sprintf "%g" prob
    in
    Printf.sprintf "discrete(%s)" (String.concat ", " (List.map format_case cases))
  | Less (te1, te2) -> Printf.sprintf "%s < %s" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | LessEq (te1, te2) -> Printf.sprintf "%s <= %s" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | And (te1, te2) ->
      Printf.sprintf "%s && %s" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | Or (te1, te2) ->
      Printf.sprintf "%s || %s" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | Not te1 ->
      Printf.sprintf "! %s" (string_of_texpr_indented ~indent te1)
  | If (te1, te2, te3) ->
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent+2) ' ' in
      let e1_str = string_of_texpr_indented ~indent te1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) te2 in
      let e3_str = string_of_texpr_indented ~indent:(indent+2) te3 in
      Printf.sprintf "if %s then\n%s%s\n%selse\n%s%s" e1_str next_indent_str e2_str indent_str next_indent_str e3_str
  | Pair (te1, te2) -> Printf.sprintf "(%s, %s)" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | First te -> Printf.sprintf "(fst %s)" (string_of_texpr_indented ~indent te)
  | Second te -> Printf.sprintf "(snd %s)" (string_of_texpr_indented ~indent te)
  | Fun (x, te) ->
      let e_str = string_of_texpr_indented ~indent:(indent+2) te in
      Printf.sprintf "fun %s -> %s" x e_str
  | App (te1, te2) -> Printf.sprintf "(%s %s)" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | FinConst (k, n) -> Printf.sprintf "int(%d,%d)" (bit_length (n-1)) k
  | FinLt (te1, te2, _) -> Printf.sprintf "%s < %s" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | FinLeq (te1, te2, _) -> Printf.sprintf "%s <= %s" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | Observe te1 -> Printf.sprintf "observe (%s)" (string_of_texpr_indented ~indent te1)
  | Fix (f, x, te) -> 
      let te_str = string_of_texpr_indented ~indent:(indent+2) te in
      Printf.sprintf "fix %s %s := %s" f x te_str
  | Nil -> "nil"
  | Cons (te1, te2) -> 
      Printf.sprintf "%s :: %s"
        (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | MatchList (te1, te_nil, y, ys, te_cons) ->
      let te1_str = string_of_texpr_indented ~indent:(indent+2) te1 in
      let te_nil_str = string_of_texpr_indented ~indent:(indent+2) te_nil in
      let te_cons_str = string_of_texpr_indented ~indent:(indent+4) te_cons in
      Printf.sprintf "match %s with\n%s  | nil -> %s\n%s  | %s :: %s -> %s\n%send"
        te1_str 
        (String.make indent ' ') te_nil_str
        (String.make indent ' ') y ys te_cons_str
        (String.make indent ' ')
  | Ref te1 -> Printf.sprintf "(ref %s)" (string_of_texpr_indented ~indent te1)
  | Deref te1 -> Printf.sprintf "(!%s)" (string_of_texpr_indented ~indent te1)
  | Assign (te1, te2) -> Printf.sprintf "(%s := %s)" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | Seq (te1, te2) -> Printf.sprintf "%s; %s" (string_of_texpr_indented ~indent te1) (string_of_texpr_indented ~indent te2)
  | Unit -> "()"

and string_of_asample ?(indent=0) dist_exp =
  match dist_exp with
  | Distr2 (DUniform, e1, e2) -> Printf.sprintf "uniform(%s, %s)" (string_of_texpr_indented ~indent e1) (string_of_texpr_indented ~indent e2)
  | Distr2 (DGaussian, e1, e2) -> Printf.sprintf "gaussian(%s, %s)" (string_of_texpr_indented ~indent e1) (string_of_texpr_indented ~indent e2)
  | Distr1 (DExponential, e1) -> Printf.sprintf "exponential(%s)" (string_of_texpr_indented ~indent e1)
  | Distr2 (DBeta, e1, e2) -> Printf.sprintf "beta(%s, %s)" (string_of_texpr_indented ~indent e1) (string_of_texpr_indented ~indent e2)
  | Distr2 (DLogNormal, e1, e2) -> Printf.sprintf "lognormal(%s, %s)" (string_of_texpr_indented ~indent e1) (string_of_texpr_indented ~indent e2)
  | Distr2 (DGamma, e1, e2) -> Printf.sprintf "gamma(%s, %s)" (string_of_texpr_indented ~indent e1) (string_of_texpr_indented ~indent e2)
  | _ -> "<other distribution>"

and string_of_ty = function
  | TBool -> "bool"
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
              in
              String.concat "," (List.map string_of_bound (BoundSet.elements bound_set))
      in
      let consts_str = 
        match FloatBag.get const_bag_ref with
        | Top -> "T"
        | Finite float_set ->
            if FloatSet.is_empty float_set then ""
            else String.concat "," (List.map (Printf.sprintf "%g") (FloatSet.elements float_set))
      in
      let content_str = 
        match bounds_str, consts_str with
        | "", "" -> ""
        | b, "" -> Printf.sprintf "[%s]" b
        | "", c -> Printf.sprintf "[; %s]" c
        | b, c  -> Printf.sprintf "[%s; %s]" b c
      in
      Printf.sprintf "float%s" content_str
  | TPair (t1, t2) -> Printf.sprintf "(%s * %s)" (string_of_ty t1) (string_of_ty t2)
  | TFun (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_ty t1) (string_of_ty t2)
  | TFin _ -> Printf.sprintf "" 
  | TUnit -> "unit"
  | TList t -> Printf.sprintf "list %s" (string_of_ty t)
  | TRef t -> Printf.sprintf "%s ref" (string_of_ty t)
  | TMeta r -> (match !r with Known t -> string_of_ty t | Unknown _ -> "?")

let string_of_expr expr = string_of_expr_indented expr 
let string_of_texpr texpr = string_of_texpr_indented texpr
let string_of_aexpr aexpr = string_of_aexpr_indented aexpr