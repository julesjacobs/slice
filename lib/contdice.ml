(* Main implementation of continuous dice *)

type expr = Types.expr =
  | Var    of string
  | Let    of string * expr * expr
  | CDistr of Stats.cdistr          (* Continuous distribution *)
  | Discrete of float list    (* list of probabilities, sum should be 1; i-th element is probability of i; returns an integer *)
  | Less   of expr * float
  | LessEq  of expr * int     (* less than or equal to; for comparing against integer outputs *)
  | Greater   of expr * float
  | GreaterEq  of expr * int     (* greater than or equal to; for comparing against integer outputs *)
  | If     of expr * expr * expr
  | Pair   of expr * expr            (* Pair construction (e1, e2) *)
  | First  of expr                   (* First projection: fst e *)
  | Second of expr                   (* Second projection: snd e *)
  | Fun    of string * expr          (* Function: fun x -> e *)
  | App    of expr * expr            (* Function application: e1 e2 *)

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
  | Greater (e, f) -> 
    Printf.sprintf "%s %s>%s %s%g%s" 
      (string_of_expr_indented ~indent e) operator_color reset_color number_color f reset_color
  | GreaterEq (e, n) -> 
    Printf.sprintf "%s %s>=%s %s%d%s" 
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

(* Wrapper for the indented pretty printer *)
let string_of_expr expr =
  string_of_expr_indented expr

(* ======== Bags as union‑find + FloatSet ======== *)

module FloatSet = Set.Make(struct
  type t = float
  let compare = compare
end)

module StringMap = Map.Make(String)

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
  | TInt
  | TMeta of ty option ref
  | TPair of ty * ty      (* t1 * t2 *)
  | TFun of ty * ty       (* t1 -> t2 *)

(* 
let new_meta () : ty =
  TMeta (ref None)
*)

let rec force t =
  match t with
  | TMeta r ->
      (match !r with
      | Some t -> force t
      | None -> t)
  | _ -> t

let rec unify (t1 : ty) (t2 : ty) : unit =
  match force t1, force t2 with
  | TBool,    TBool      -> ()
  | TFloat b1, TFloat b2 -> assert_eq b1 b2
  | TInt,     TInt       -> ()
  | TPair(a1, b1), TPair(a2, b2) -> 
      unify a1 a2; 
      unify b1 b2
  | TFun(a1, b1), TFun(a2, b2) -> 
      unify a1 a2; 
      unify b1 b2
  | TBool,    TFloat _   -> failwith "Type error: expected bool, got float"
  | TBool,    TInt       -> failwith "Type error: expected bool, got int"
  | TBool,    TPair _    -> failwith "Type error: expected bool, got pair"
  | TBool,    TFun _     -> failwith "Type error: expected bool, got function"
  | TFloat _, TBool      -> failwith "Type error: expected float, got bool"
  | TFloat _, TInt       -> failwith "Type error: expected float, got int"
  | TFloat _, TPair _    -> failwith "Type error: expected float, got pair"
  | TFloat _, TFun _     -> failwith "Type error: expected float, got function"
  | TInt,     TBool      -> failwith "Type error: expected int, got bool"
  | TInt,     TFloat _   -> failwith "Type error: expected int, got float"
  | TInt,     TPair _    -> failwith "Type error: expected int, got pair"
  | TInt,     TFun _     -> failwith "Type error: expected int, got function"
  | TPair _,  TBool      -> failwith "Type error: expected pair, got bool"
  | TPair _,  TFloat _   -> failwith "Type error: expected pair, got float"
  | TPair _,  TInt       -> failwith "Type error: expected pair, got int"
  | TPair _,  TFun _     -> failwith "Type error: expected pair, got function"
  | TFun _,   TBool      -> failwith "Type error: expected function, got bool"
  | TFun _,   TFloat _   -> failwith "Type error: expected function, got float"
  | TFun _,   TInt       -> failwith "Type error: expected function, got int"
  | TFun _,   TPair _    -> failwith "Type error: expected function, got pair"
  | TMeta r1, _   ->
      r1 := Some t2
  | _, TMeta r2   ->
      r2 := Some t1

(* ======== Annotated expressions ======== *)

type texpr = ty * aexpr
and aexpr =
  | Var     of string
  | Let     of string * texpr * texpr
  | CDistr  of Stats.cdistr
  | Discrete of float list
  | Less    of texpr * float
  | LessEq   of texpr * int
  | Greater    of texpr * float
  | GreaterEq   of texpr * int
  | If      of texpr * texpr * texpr
  | Pair    of texpr * texpr
  | First   of texpr
  | Second  of texpr
  | Fun     of string * texpr
  | App     of texpr * texpr

(* Elaborator: expr -> (ty * aexpr), generating bag constraints *)
let elab (e : expr) : texpr =
  let rec aux (env : ty StringMap.t) (e : expr) : texpr =
    match e with
    | Var x ->
      (try 
        (StringMap.find x env, Var x)
       with Not_found -> 
        failwith ("Unbound variable: " ^ x))

    | Let (x, e1, e2) ->
      let t1, a1 = aux env e1 in
      let env' = StringMap.add x t1 env in
      let t2, a2 = aux env' e2 in
      (t2, Let (x, (t1,a1), (t2,a2)))

    | CDistr dist ->
      let b = new_bag () in
      (TFloat b, CDistr dist)
      
    | Discrete probs ->
      let sum = List.fold_left (+.) 0.0 probs in
      if abs_float (sum -. 1.0) > 0.0001 then
        failwith (Printf.sprintf "Discrete distribution probabilities must sum to 1.0, got %f" sum);
      
      (* Discrete distributions return an integer type *)
      (TInt, Discrete probs)

    | Less (e1, f) ->
      let t1, a1 = aux env e1 in
      (* enforce e1 : float and record f ∈ its bag *)
      let b = new_bag () in
      unify t1 (TFloat b);
      assert_elem f b;
      (TBool, Less ((t1,a1), f))
      
    | LessEq (e1, n) ->
      let t1, a1 = aux env e1 in
      (* For LessEq, the expression must be of integer type *)
      unify t1 TInt;
      (TBool, LessEq ((t1,a1), n))

    | Greater (e1, f) ->
      let t1, a1 = aux env e1 in
      (* enforce e1 : float and record f ∈ its bag *)
      let b = new_bag () in
      unify t1 (TFloat b);
      assert_elem f b;
      (TBool, Greater ((t1,a1), f))
      
    | GreaterEq (e1, n) ->
      let t1, a1 = aux env e1 in
      (* For GreaterEq, the expression must be of integer type *)
      unify t1 TInt;
      (TBool, GreaterEq ((t1,a1), n))

    | If (e1, e2, e3) ->
      let t1, a1 = aux env e1 in
      unify t1 TBool;
      let t2, a2 = aux env e2 in
      let t3, a3 = aux env e3 in
      unify t2 t3;
      (t2, If ((t1,a1), (t2,a2), (t3,a3)))
      
    | Pair (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (TPair (t1, t2), Pair ((t1, a1), (t2, a2)))
      
    | First e ->
      let t, a = aux env e in
      let t1 = TMeta (ref None) in
      let t2 = TMeta (ref None) in
      unify t (TPair (t1, t2));
      (t1, First (t, a))
      
    | Second e ->
      let t, a = aux env e in
      let t1 = TMeta (ref None) in
      let t2 = TMeta (ref None) in
      unify t (TPair (t1, t2));
      (t2, Second (t, a))
      
    | Fun (x, e) ->
      let param_type = TMeta (ref None) in
      let env' = StringMap.add x param_type env in
      let return_type, a = aux env' e in
      (TFun (param_type, return_type), Fun (x, (return_type, a)))
      
    | App (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let result_type = TMeta (ref None) in
      unify t1 (TFun (t2, result_type));
      (result_type, App ((t1, a1), (t2, a2)))
  in

  aux StringMap.empty e

(* Pretty printer for types with colors *)
let type_color = "\027[1;35m"    (* Bold Magenta *)
let bracket_color = "\027[1;36m" (* Bold Cyan *)

let rec string_of_ty = function
  | TBool -> Printf.sprintf "%sbool%s" type_color reset_color
  | TInt -> Printf.sprintf "%sint%s" type_color reset_color
  | TFloat bag ->
      let root = find bag in
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

(* Function that does elab but insists that the return type is TBool *)
let elab_bool (e : expr) : texpr =
  let t, a = elab e in
  match force t with
  | TBool -> (t, a)
  | _ -> failwith (Printf.sprintf "Type error: expected bool, got %s" (string_of_ty t))

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
  | CDistr dist -> string_of_cdistr dist
  | Discrete probs -> 
      Printf.sprintf "%sdiscrete%s(%s%s%s)" 
        keyword_color reset_color number_color
        (String.concat ", " (List.map (fun f -> Printf.sprintf "%g" f) probs))
        reset_color
  | Less (e, f) -> 
      Printf.sprintf "%s %s<%s %s%g%s" 
        (string_of_texpr_indented ~indent e) operator_color reset_color number_color f reset_color
  | LessEq (e, n) -> 
      Printf.sprintf "%s %s<=%s %s%d%s" 
        (string_of_texpr_indented ~indent e) operator_color reset_color number_color n reset_color
  | Greater (e, f) -> 
    Printf.sprintf "%s %s>%s %s%g%s" 
      (string_of_texpr_indented ~indent e) operator_color reset_color number_color f reset_color
  | GreaterEq (e, n) -> 
    Printf.sprintf "%s %s>=%s %s%d%s" 
      (string_of_texpr_indented ~indent e) operator_color reset_color number_color n reset_color
  | If (e1, e2, e3) -> 
      let indent_str = String.make indent ' ' in
      let next_indent_str = String.make (indent+2) ' ' in
      let e1_str = string_of_texpr_indented ~indent e1 in
      let e2_str = string_of_texpr_indented ~indent:(indent+2) e2 in
      let e3_str = string_of_texpr_indented ~indent:(indent+2) e3 in
      Printf.sprintf "%sif%s %s %sthen%s\n%s%s\n%s%selse%s\n%s%s" 
        keyword_color reset_color e1_str keyword_color reset_color 
        next_indent_str e2_str indent_str keyword_color reset_color next_indent_str e3_str
  | Pair (e1, e2) ->
      let e1_str = string_of_texpr_indented ~indent e1 in
      let e2_str = string_of_texpr_indented ~indent e2 in
      Printf.sprintf "(%s, %s)" e1_str e2_str
  | First e ->
      let e_str = string_of_texpr_indented ~indent e in
      Printf.sprintf "%sfst%s %s" keyword_color reset_color e_str
  | Second e ->
      let e_str = string_of_texpr_indented ~indent e in
      Printf.sprintf "%ssnd%s %s" keyword_color reset_color e_str
  | Fun (x, e) ->
      let e_str = string_of_texpr_indented ~indent:(indent+2) e in
      Printf.sprintf "%sfun%s %s%s%s %s->%s %s" 
        keyword_color reset_color variable_color x reset_color 
        operator_color reset_color e_str
  | App (e1, e2) ->
      let e1_str = string_of_texpr_indented ~indent e1 in
      let e2_str = string_of_texpr_indented ~indent e2 in
      Printf.sprintf "%s %s" e1_str e2_str

(* Wrappers for the indented pretty printers *)
let string_of_texpr expr =
  string_of_texpr_indented expr

let string_of_aexpr aexpr =
  string_of_aexpr_indented aexpr

(* Calculate probability for a given distribution in an interval *)
let prob_cdistr_interval (left : float) (right : float) (dist : Stats.cdistr) : float =
  let cdf = Stats.cdistr_cdf dist in
  cdf right -. cdf left

(* 
Discretizer from typed expressions to discrete expressions.

The idea is that the type system infers a bag of floats that each 
expression possibly compares against. Instead of sampling from a continuous 
distribution, we sample from a discrete distribution that tells us the 
probabilities of the interval between two floats.

When doing a comparison against a float, we convert that to a comparison 
against the discrete integer that represents the i-th float in the bag.
*)
let discretize (e : texpr) : expr =
  let rec aux ((ty, ae) : texpr) : expr =
    match ae with
    | Var x ->
        Var x

    | Let (x, te1, te2) ->
        Let (x, aux te1, aux te2)

    | CDistr dist ->
        let b =
          match ty with TFloat b -> b | _ -> failwith "Continuous distribution must be float"
        in
        (* GLOBAL sorted list of *all* cut‑points for this bag *)
        let cuts =
          match !(find b) with
          | Root { elems } -> FloatSet.elements elems
          | Link _         -> assert false
        in
        (* Compute set of intervals starting at -infty and ending at +infty *)
        let intervals = List.init (List.length cuts + 1) (fun i ->
          let left = if i = 0 then neg_infinity else List.nth cuts (i - 1) in
          let right = if i = List.length cuts then infinity else List.nth cuts i in
          (left, right)
        ) in
        let probs = List.map (fun (left, right) ->
          prob_cdistr_interval left right dist
        ) intervals in
        Discrete probs
        
    | Discrete probs ->
        (* For integer types, we pass through the discrete distribution directly *)
        Discrete probs

    | Less ((t_sub, ae_sub), f) ->
        let d_sub = aux (t_sub, ae_sub) in
        (* compute threshold index by counting all cut‑points < f *)
        let cuts =
          match force t_sub with 
          | TFloat b -> 
              (match !(find b) with
              | Root { elems } -> FloatSet.elements elems
              | Link _         -> assert false)
          | _ -> failwith "Less must be float"
        in
        let idx = List.length (List.filter (fun x -> x < f) cuts) in
        LessEq (d_sub, idx)
        
    | LessEq ((t_sub, ae_sub), n) ->
        let d_sub = aux (t_sub, ae_sub) in
        (* For integer LessEq comparisons, we pass them through unchanged *)
        LessEq (d_sub, n)

    | Greater ((t_sub, ae_sub), f) ->
      let d_sub = aux (t_sub, ae_sub) in
      (* compute threshold index by counting all cut‑points > f *)
      let cuts =
        match force t_sub with 
        | TFloat b -> 
            (match !(find b) with
            | Root { elems } -> FloatSet.elements elems
            | Link _         -> assert false)
        | _ -> failwith "Greater must be float"
      in
      let idx = List.length (List.filter (fun x -> x > f) cuts) in
      GreaterEq (d_sub, (List.length cuts) - idx)
      
    | GreaterEq ((t_sub, ae_sub), n) ->
      let d_sub = aux (t_sub, ae_sub) in
      (* For integer GreaterEq comparisons, we pass them through unchanged *)
      GreaterEq (d_sub, n)

    | If ((t1, ae1), (t2, ae2), (t3, ae3)) ->
        If (aux (t1, ae1), aux (t2, ae2), aux (t3, ae3))
        
    | Pair (te1, te2) ->
        Pair (aux te1, aux te2)
        
    | First te ->
        First (aux te)
        
    | Second te ->
        Second (aux te)
        
    | Fun (x, te) ->
        Fun (x, aux te)
        
    | App (te1, te2) ->
        App (aux te1, aux te2)
  in
  aux e