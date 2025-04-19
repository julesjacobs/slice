(* Interface for continuous dice *)

(* Re-export types from Types module *)
type expr = Types.expr =
  | Var of string
  | Let of string * expr * expr
  | Uniform of float * float
  | Less of expr * float
  | If of expr * expr * expr

(* FloatSet module *)
module FloatSet : Set.S with type elt = float

(* Bag-related types *)
type bag_contents =
  | Root of { mutable elems : FloatSet.t }
  | Link of bag

and bag = bag_contents ref

(* Type definitions *)
type ty =
  | TBool
  | TFloat of bag

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

(* Parsing *)
val parse_expr : string -> expr

(* Discrete expressions *)
type dexpr =
  | Var    of string
  | Let    of string * dexpr * dexpr
  | Discrete of float list 
  | LessEq   of dexpr * int
  | If     of dexpr * dexpr * dexpr

(* Pretty printing *)
val string_of_expr : expr -> string
val string_of_ty : ty -> string
val string_of_texpr : texpr -> string
val string_of_aexpr : aexpr -> string
val string_of_dexpr : dexpr -> string

(* Elaboration *)
val elab : expr -> texpr
val elab_bool : expr -> texpr

(* Compilation *)
val compile : texpr -> dexpr