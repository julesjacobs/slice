(* Type system and unification types *)

open Bags (* For BoundBag.bag and FloatBag.bag *)

type meta = Unknown of (ty -> unit) list | Known of ty
and meta_ref = meta ref

and ty =
  | TBool
  | TFloat of
      BoundBag.bag * FloatBag.bag (* Store bag REFERENCES, not contents *)
  | TPair of ty * ty (* t1 * t2 *)
  | TFun of ty * ty (* t1 -> t2 *)
  | TFin of int (* Represents Z_n, integers modulo n *)
  | TMeta of meta_ref (* Type variable for unification *)
  | TUnit
  | TList of ty (* list t *)
  | TRef of ty (* t ref *)

(** Function to recursively dereference type variables *)
val force : ty -> ty

(** Function to add a listener to a meta variable *)
val listen : meta_ref -> (ty -> unit) -> unit

(** Function to create a fresh meta variable *)
val fresh_meta : unit -> ty

(** Function to assign a type to a meta variable *)
val assign : meta_ref -> ty -> unit
