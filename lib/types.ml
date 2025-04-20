(* Type definitions for ContDice *)

(* Base functor for expression structure *)
type 'a expr_generic = 
  | Var    of string
  | Let    of string * 'a * 'a
  | CDistr of Stats.cdistr          (* Continuous distribution *)
  | Discrete of float list    (* list of probabilities, sum should be 1; i-th element is probability of i *)
  | Less   of 'a * float
  | LessEq  of 'a * int
  | If     of 'a * 'a * 'a
  | Pair   of 'a * 'a            (* Pair construction (e1, e2) *)
  | First  of 'a                   (* First projection: fst e *)
  | Second of 'a                   (* Second projection: snd e *)
  | Fun    of string * 'a          (* Function: fun x -> e *)
  | App    of 'a * 'a            (* Function application: e1 e2 *)

(* The source language expression type - Wrapped *)
type expr = ExprNode of expr expr_generic

(* Type definitions for the typed language *)

type ty =
  | TBool
  | TFloat of Bag.bag (* Use Bag.bag *)
  | TInt
  | TMeta of ty option ref
  | TPair of ty * ty      (* t1 * t2 *)
  | TFun of ty * ty       (* t1 -> t2 *)

(* Typed expressions (recursive definition with aexpr) - Wrapped *)

type texpr = ty * aexpr
and aexpr = TAExprNode of texpr expr_generic (* Annotated expression uses texpr recursively, wrapped *)