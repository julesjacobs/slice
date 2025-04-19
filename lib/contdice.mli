(* Interface for continuous dice *)

type expr = Types.expr =
  | Var of string
  | Let of string * expr * expr
  | CDistr of Stats.cdistr          (* Continuous distribution *)
  | Discrete of float list    (* list of probabilities, sum should be 1; i-th element is probability of i *)
  | Less of expr * float
  | LessEq of expr * int
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
  | TInt
  | TMeta of ty option ref

(* Typed expressions *)
type texpr = ty * aexpr
and aexpr =
  | Var     of string
  | Let     of string * texpr * texpr
  | CDistr  of Stats.cdistr
  | Discrete of float list
  | Less    of texpr * float
  | LessEq   of texpr * int
  | If      of texpr * texpr * texpr

(* Bag operations *)
val new_bag : unit -> bag
val find : bag -> bag
val assert_eq : bag -> bag -> unit
val assert_elem : float -> bag -> unit
val unify : ty -> ty -> unit

(* Parsing *)
val parse_expr : string -> expr


(* Pretty printing *)
val string_of_expr : expr -> string
val string_of_ty : ty -> string
val string_of_texpr : texpr -> string
val string_of_aexpr : aexpr -> string

(* Elaboration *)
val elab : expr -> texpr
val elab_bool : expr -> texpr

(* Compilation *)
val discretize : texpr -> expr