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

(* The source language expression type *)
type expr = ExprNode of expr expr_generic

(* Type definitions for the typed language *)

open Bags

type ty =
  | TBool
  | TFloat of BoundBag.bag * FloatBag.bag
  | TMeta of ty option ref
  | TPair of ty * ty      (* t1 * t2 *)
  | TFun of ty * ty       (* t1 -> t2 *)

(* Typed expressions (recursive definition with aexpr) *)

type texpr = ty * aexpr
and aexpr = TAExprNode of texpr expr_generic