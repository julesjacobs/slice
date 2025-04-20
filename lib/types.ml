(* Type definitions for ContDice *)

(* Base functor for expression structure *)
type 'a expr_generic = 
  | Var    of string
  | Const  of float
  | Let    of string * 'a * 'a
  | CDistr of Stats.cdistr          (* Continuous distribution *)
  | Discrete of float list    (* list of probabilities; i-th element is probability of float(i) *)
  | Less   of 'a * float
  | LessEq  of 'a * float
  | If     of 'a * 'a * 'a
  | Pair   of 'a * 'a            (* Pair construction (e1, e2) *)
  | First  of 'a                   (* First projection: fst e *)
  | Second of 'a                   (* Second projection: snd e *)
  | Fun    of string * 'a          (* Function: fun x -> e *)
  | App    of 'a * 'a            (* Function application: e1 e2 *)
  | FinConst of int * int (* k, n for k#n *)
  | FinLt of 'a * 'a * int (* e1 <#n e2 *)
  | FinLeq of 'a * 'a * int (* e1 <=#n e2 *)

(* The source language expression type *)
type expr = ExprNode of expr expr_generic

(* Type definitions for the typed language *)

open Bags

type ty =
  | TBool
  | TFloat of BoundBag.bag * FloatBag.bag (* Store bag REFERENCES, not contents *)
  | TPair of ty * ty      (* t1 * t2 *)
  | TFun of ty * ty       (* t1 -> t2 *)
  | TFin of int (* Represents Z_n, integers modulo n *)
  | TMeta of ty option ref (* Type variable for unification *)

(* Function to recursively dereference type variables *)
let rec force t =
  match t with
  | TMeta r ->
      (match !r with
      | Some t' -> force t' (* Recursively force the resolved type *)
      | None -> t (* Return the TMeta itself if it's unresolved *))
  | _ -> t (* Return the type if it's not a TMeta *)

(* Typed expressions (recursive definition with aexpr) *)

type texpr = ty * aexpr
and aexpr = TAExprNode of texpr expr_generic

(* Remove the duplicated definitions below *)
(*
(* Abstract syntax tree for expressions *)
type expr_node =
  | Const of float
  ...
and expr = ExprNode of expr_node

(* Annotated (typed) expressions *)
type aexpr_node =
  | Const of float
  ...
and aexpr = TAExprNode of aexpr_node
and texpr = ty * aexpr (* Type-annotated expression *)
*)