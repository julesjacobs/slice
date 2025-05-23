(**
 * @file type_system.ml
 * @brief Defines the core types and operations for the ContDice type system.
 *
 * This module includes definitions for types (`ty`), meta type variables (`meta`, `meta_ref`)
 * used in unification, and functions for manipulating these types (e.g., `force`,
 * `listen`, `fresh_meta`, `assign`). It relies on the `Bags` module for handling
 * sets of constraints on float types.
 *)

open Bags (* For BoundBag.bag and FloatBag.bag *)

(** Represents the state of a meta type variable: either unknown (unresolved)
    with a list of listeners, or known (resolved to a concrete type). *)
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

(** Recursively dereferences a type variable until a concrete type or an
    unresolved meta variable is found. This is crucial for finding the
    actual type represented by a `TMeta` that might point to another `TMeta`. *)
let rec force t =
  match t with
  | TMeta r -> (
      match !r with
      | Known t' -> force t' (* Recursively force the resolved type *)
      | Unknown _ -> t (* Return the TMeta itself if it's unresolved (already forced) *))
  | _ -> t (* Not a TMeta, so it's already forced *)

(** Adds a listener function `f` to a meta variable `m`.
    If `m` is already known, `f` is executed immediately with the known type.
    Otherwise, `f` is added to the list of listeners to be executed when `m` is assigned. *)
let listen (m : meta_ref) (f : ty -> unit) : unit =
  match !m with
  | Known t -> f t (* Execute immediately if known *)
  | Unknown fs -> m := Unknown (f :: fs) (* Add to listeners if unknown *)

(** Creates a new, fresh meta type variable, initialized to `Unknown`. *)
let fresh_meta () : ty = TMeta (ref (Unknown []))

(** Assigns a concrete type `t` to a meta variable `m`.
    If `m` is already known, this operation results in an error (failwith).
    After assigning, all registered listeners for `m` are executed with the assigned type `t`. *)
let assign (m : meta_ref) (t : ty) : unit =
  match !m with
  | Known _ -> failwith "Cannot assign to an already known type variable"
  | Unknown fs ->
      m := Known t;
      List.iter (fun f -> f t) fs
