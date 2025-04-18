(* Main implementation of continuous dice *)

(* Re-export types from Types module *)
type expr = Types.expr =
  | Var    of string
  | Let    of string * expr * expr
  | Uniform of float * float
  | Less   of expr * float
  | If     of expr * expr * expr

(* Parser for expressions *)
let parse_expr (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.LexError msg -> failwith ("Lexical error: " ^ msg)
  | Parser.Error -> 
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol in
      let token = Lexing.lexeme lexbuf in
      failwith (Printf.sprintf "Parse error at line %d, column %d: unexpected token '%s'" line col token)

(* ANSI color codes for syntax highlighting *)
let keyword_color = "\027[1;34m"  (* Bold Blue *)
let operator_color = "\027[1;31m" (* Bold Red *)
let number_color = "\027[0;32m"   (* Green *)
let variable_color = "\027[0;33m" (* Yellow *)
let reset_color = "\027[0m"       (* Reset *)

(* Pretty printers with indentation and colors *)
let rec string_of_expr_indented ?(indent=0) = function
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, e1, e2) -> 
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_expr_indented ~indent:(indent+2) e1 in
      let e2_str = string_of_expr_indented ~indent:(indent+2) e2 in
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s" 
        keyword_color reset_color variable_color x reset_color e1_str 
        keyword_color reset_color indent_str e2_str
  | Uniform (lo, hi) -> 
      Printf.sprintf "%suniform%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color lo reset_color number_color hi reset_color
  | Less (e, f) -> 
      Printf.sprintf "%s %s<%s %s%g%s" 
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

(* Wrapper for the indented pretty printer *)
let string_of_expr expr =
  string_of_expr_indented expr

(* ======== Bags as union‑find + FloatSet ======== *)

module FloatSet = Set.Make(struct
  type t = float
  let compare = compare
end)

(* A bag is a ref to either:
   - Root { elems }  : the canonical node holding a set of floats
   - Link parent     : pointing up to another bag
*)
type bag_contents =
  | Root of { mutable elems : FloatSet.t }
  | Link of bag

and bag = bag_contents ref

let new_bag () : bag =
  ref (Root { elems = FloatSet.empty })

(* Find with path compression *)
let rec find (b : bag) : bag =
  match !b with
  | Root _ -> b
  | Link parent ->
      let root = find parent in
      b := Link root;
      root

(* Union two bags, merging their FloatSets at the new root *)
let assert_eq (b1 : bag) (b2 : bag) : unit =
  let r1 = find b1
  and r2 = find b2 in
  if r1 != r2 then (
    match !r1, !r2 with
    | Root data1, Root data2 ->
        (* merge into r1 *)
        data1.elems <- FloatSet.union data1.elems data2.elems;
        (* clear the old root and link it under r1 *)
        data2.elems <- FloatSet.empty;
        r2 := Link r1
    | _ ->
        assert false  (* impossible: after find both must be Root *)
  )

(* Record that float x ∈ bag *)
let assert_elem (x : float) (b : bag) : unit =
  let r = find b in
  match !r with
  | Root data -> data.elems <- FloatSet.add x data.elems
  | Link _ -> assert false  (* impossible after find *)

(* ======== Types and unification ======== *)

type ty =
  | TBool
  | TFloat of bag

let unify (t1 : ty) (t2 : ty) : unit =
  match t1, t2 with
  | TBool,    TBool      -> ()
  | TFloat b1, TFloat b2 -> assert_eq b1 b2
  | TBool,    TFloat _   -> failwith "Type error: expected bool, got float"
  | TFloat _, TBool      -> failwith "Type error: expected float, got bool"

(* ======== Annotated expressions ======== *)

type texpr = ty * aexpr
and aexpr =
  | Var     of string
  | Let     of string * texpr * texpr
  | Uniform of float * float
  | Less    of texpr * float
  | If      of texpr * texpr * texpr

(* Elaborator: expr -> (ty * aexpr), generating bag constraints *)
let elab (e : expr) : texpr =
  let env : (string, texpr) Hashtbl.t = Hashtbl.create 16 in

  let rec aux (e : expr) : texpr =
    match e with
    | Var x ->
      (try 
         Hashtbl.find env x
       with Not_found -> 
         (* Try to parse as a number *)
         try 
           let n = float_of_string x in
           let b = new_bag () in
           assert_elem n b;
           (TFloat b, Uniform (n, n))  (* Represent constants as uniform(n,n) *)
         with _ -> 
           failwith ("Unbound variable: " ^ x))

    | Let (x, e1, e2) ->
      let t1, a1 = aux e1 in
      Hashtbl.add env x (t1, a1);
      let t2, a2 = aux e2 in
      (t2, Let (x, (t1,a1), (t2,a2)))

    | Uniform (lo, hi) ->
      let b = new_bag () in
      (TFloat b, Uniform (lo, hi))

    | Less (e1, f) ->
      let t1, a1 = aux e1 in
      (* enforce e1 : float and record f ∈ its bag *)
      let b = new_bag () in
      unify t1 (TFloat b);
      assert_elem f b;
      (TBool, Less ((t1,a1), f))

    | If (e1, e2, e3) ->
      let t1, a1 = aux e1 in
      unify t1 TBool;
      let t2, a2 = aux e2 in
      let t3, a3 = aux e3 in
      unify t2 t3;
      (t2, If ((t1,a1), (t2,a2), (t3,a3)))
  in

  aux e

(* Pretty printer for types with colors *)
let type_color = "\027[1;35m"    (* Bold Magenta *)
let bracket_color = "\027[1;36m" (* Bold Cyan *)

let string_of_ty = function
  | TBool -> Printf.sprintf "%sbool%s" type_color reset_color
  | TFloat bag ->
      let root = find bag in
      match !root with
      | Root { elems } ->
          if FloatSet.is_empty elems then
            Printf.sprintf "%sfloat%s" type_color reset_color
          else
            let elements = FloatSet.elements elems in
            let str_elems = String.concat ", " 
              (List.map (fun f -> Printf.sprintf "%s%g%s" number_color f reset_color) elements) in
            Printf.sprintf "%sfloat%s%s<%s%s>%s" 
              type_color reset_color bracket_color str_elems bracket_color reset_color
      | Link _ -> failwith "Impossible: find returned a Link"

(* Pretty printer for typed expressions with indentation and colors *)
let paren_color = "\027[1;37m"   (* Bold White *)

let rec string_of_texpr_indented ?(indent=0) ((ty, aexpr) : texpr) : string =
  let aexpr_str = string_of_aexpr_indented ~indent aexpr in
  Printf.sprintf "%s(%s%s : %s%s)%s" 
    paren_color reset_color aexpr_str (string_of_ty ty) paren_color reset_color

and string_of_aexpr_indented ?(indent=0) = function
  | Var x -> Printf.sprintf "%s%s%s" variable_color x reset_color
  | Let (x, e1, e2) -> 
      let indent_str = String.make indent ' ' in
      let e1_str = string_of_texpr_indented ~indent:(indent+2) e1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) e2 in
      Printf.sprintf "%slet%s %s%s%s = %s %sin%s\n%s%s" 
        keyword_color reset_color variable_color x reset_color e1_str 
        keyword_color reset_color indent_str e2_str
  | Uniform (lo, hi) -> 
      Printf.sprintf "%suniform%s(%s%g%s, %s%g%s)" 
        keyword_color reset_color number_color lo reset_color number_color hi reset_color
  | Less (e, f) -> 
      Printf.sprintf "%s %s<%s %s%g%s" 
        (string_of_texpr_indented ~indent e) operator_color reset_color number_color f reset_color
  | If (e1, e2, e3) -> 
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent+2) ' ' in
      let e1_str = string_of_texpr_indented ~indent e1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) e2 in
      let e3_str = string_of_texpr_indented ~indent:(indent+2) e3 in
      Printf.sprintf "%sif%s %s %sthen%s\n%s%s\n%s%selse%s\n%s%s" 
        keyword_color reset_color e1_str keyword_color reset_color 
        next_indent_str e2_str indent_str keyword_color reset_color next_indent_str e3_str

(* Wrappers for the indented pretty printers *)
let string_of_texpr expr =
  string_of_texpr_indented expr

let string_of_aexpr aexpr =
  string_of_aexpr_indented aexpr

(* A small test helper *)
let hello name =
  Printf.printf "Hello, %s!\n" name