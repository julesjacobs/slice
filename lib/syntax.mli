(* Interface for the syntax module *)

(* The source language *)
type expr =
  | Var of string
  | Let of string * expr * expr
  | Uniform of float * float
  | Less of expr * float
  | If of expr * expr * expr

module FloatSet : Set.S with type elt = float

(* Type definitions *)
type ty =
  | TBool
  | TFloat of bag

and bag = bag_contents ref
and bag_contents = 
  | Root of { mutable elems : FloatSet.t }
  | Link of bag

(* Typed expressions *)
type texpr = ty * aexpr
and aexpr =
  | Var     of string
  | Let     of string * texpr * texpr
  | Uniform of float * float
  | Less    of texpr * float
  | If      of texpr * texpr * texpr

(* Bag operations *)
val new_bag : unit -> bag
val find : bag -> bag
val assert_eq : bag -> bag -> unit
val assert_elem : float -> bag -> unit
val unify : ty -> ty -> unit