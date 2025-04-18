(* Type definitions for ContDice *)

(* The source language *)
type expr =
  | Var    of string
  | Let    of string * expr * expr
  | Uniform of float * float
  | Less   of expr * float
  | If     of expr * expr * expr