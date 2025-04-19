(* Type definitions for ContDice *)

(* The source language *)
type expr =
  | Var    of string
  | Let    of string * expr * expr
  | Uniform of float * float
  | Discrete of float list  (* list of probabilities, sum should be 1; i-th element is probability of i *)
  | Less   of expr * float
  | LessEq  of expr * int
  | If     of expr * expr * expr