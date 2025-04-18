(* Interface for continuous dice *)

type expr =
  | Var of string
  | Let of string * expr * expr
  | Uniform of float * float
  | Less of expr * float
  | If of expr * expr * expr

val hello : string -> unit
(** Prints a greeting to the given name. *)