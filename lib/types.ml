(* Type definitions for ContDice *)

(* The source language *)
type expr =
  | Var    of string
  | Let    of string * expr * expr
  | CDistr of Stats.cdistr          (* Continuous distribution *)
  | Discrete of float list    (* list of probabilities, sum should be 1; i-th element is probability of i *)
  | Less   of expr * float
  | LessEq  of expr * int
  | If     of expr * expr * expr
  | Pair   of expr * expr            (* Pair construction (e1, e2) *)
  | First  of expr                   (* First projection: fst e *)
  | Second of expr                   (* Second projection: snd e *)
  | Fun    of string * expr          (* Function: fun x -> e *)
  | App    of expr * expr            (* Function application: e1 e2 *)