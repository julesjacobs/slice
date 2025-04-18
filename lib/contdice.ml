(* Main implementation of continuous dice *)

type expr =
  | Var of string
  | Let of string * expr * expr
  | Uniform of float * float
  | Less of expr * float
  | If of expr * expr * expr

(* 
A bag is eventually a list of floats. We solve constraints over bags.
The constraints are:
- Equality constraints: bag1 = bag2
- Element of constraints: float x in bag1
These constraints are solved similarly to Hindley-Milner type inference / unification.
*)
type bag = ???

(* Functions to assert constraints over bags *)
let assert_eq (bag1 : bag) (bag2 : bag) : unit =
  (* TODO *)
  ()

let assert_elem (x : float) (bag : bag) : unit =
  (* TODO *)
  ()

type ty =
  | TBool
  | TFloat of bag

let unify (t1 : ty) (t2 : ty) : unit =
  (* TODO *)
  ()

type texpr = (ty * aexpr)
and aexpr =
  | Var of string
  | Let of string * texpr * texpr
  | Uniform of float * float
  | Less of texpr * float
  | If of texpr * texpr * texpr

(* 
  An elaborator that turns an expression into an annotated expression.
  This function creates constraints over bags, so that a type TFloat bag can only be used
  in Less comparisons if the constant is an element of the bag.
  For the other constructs we use unification of bags similar to Hindley-Milner type inference.
*)
let rec elab (e : expr) : texpr =
  (* TODO *)
  (TBool, e)

let hello name =
  Printf.printf "Hello, %s!\n" name