(* Type definitions for ContDice *)

(* Base functor for expression structure *)
type 'a expr_generic = 
  | Var    of string
  | Const  of float
  | Let    of string * 'a * 'a
  | CDistr of Stats.cdistr          (* Continuous distribution *)
  (* | Discrete of float list    (* list of probabilities; i-th element is probability of float(i) *) *)
  | DistrCase of ('a * float) list (* General discrete distribution: (expr * prob) list *)
  | Less   of 'a * 'a
  | LessEq of 'a * 'a
  | If     of 'a * 'a * 'a
  | Pair   of 'a * 'a            (* Pair construction (e1, e2) *)
  | First  of 'a                   (* First projection: fst e *)
  | Second of 'a                   (* Second projection: snd e *)
  | Fun    of string * 'a          (* Function: fun x -> e *)
  | App    of 'a * 'a            (* Function application: e1 e2 *)
  | FinConst of int * int (* k, n for k#n *)
  | FinLt of 'a * 'a * int (* e1 <#n e2 *)
  | FinLeq of 'a * 'a * int (* e1 <=#n e2 *)

(* The source language expression type *)
type expr = ExprNode of expr expr_generic

(* Type definitions for the typed language *)

open Bags

type meta =
  | Unknown of (ty -> unit) list
  | Known of ty
and meta_ref = meta ref
and ty =
  | TBool
  | TFloat of BoundBag.bag * FloatBag.bag (* Store bag REFERENCES, not contents *)
  | TPair of ty * ty      (* t1 * t2 *)
  | TFun of ty * ty       (* t1 -> t2 *)
  | TFin of int (* Represents Z_n, integers modulo n *)
  | TMeta of meta_ref (* Type variable for unification *)

(* Function to recursively dereference type variables *)
let rec force t =
  match t with
  | TMeta r ->
      (match !r with
      | Known t' -> force t' (* Recursively force the resolved type *)
      | Unknown _ -> t (* Return the TMeta itself if it's unresolved *))
  | _ -> t (* Return the type if it's not a TMeta *)

let listen (m : meta_ref) (f : ty -> unit) : unit =
  match !m with
  | Known t -> f t
  | Unknown fs -> m := Unknown (f :: fs)

let fresh_meta () : ty = TMeta (ref (Unknown []))

let assign (m : meta_ref) (t : ty) : unit =
  match !m with
  | Known _ -> failwith "Cannot assign to a known type"
  | Unknown fs -> m := Known t; List.iter (fun f -> f t) fs

(* Typed expressions (recursive definition with aexpr) *)

type texpr = ty * aexpr
and aexpr = TAExprNode of texpr expr_generic

(* Remove the duplicated definitions below *)
(*
(* Abstract syntax tree for expressions *)
type expr_node =
  | Const of float
  ...
and expr = ExprNode of expr_node

(* Annotated (typed) expressions *)
type aexpr_node =
  | Const of float
  ...
and aexpr = TAExprNode of aexpr_node
and texpr = ty * aexpr (* Type-annotated expression *)
*)

(* Runtime values *)
type value =
  | VBool of bool
  | VFloat of float
  | VPair of value * value
  | VFin of int * int (* value k, modulus n *)
  | VClosure of string * expr * env
and env = (string * value) list (* Simple association list for environment *)

(* Helper for pretty printing values *)
let rec string_of_value = function
  | VBool b -> string_of_bool b
  | VFloat f -> string_of_float f
  | VPair (v1, v2) -> Printf.sprintf "(%s, %s)" (string_of_value v1) (string_of_value v2)
  | VFin (k, n) -> Printf.sprintf "%d#%d" k n
  | VClosure (x, _, _) -> Printf.sprintf "<fun %s>" x