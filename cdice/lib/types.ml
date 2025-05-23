(* Type definitions for ContDice *)

(* Comparison operators *)
type cmp_op = Lt | Le | Gt | Ge

(* Base functor for expression structure *)
type 'a expr_generic = 
  | Var    of string
  | Const  of float
  | BoolConst of bool            
  | Let    of string * 'a * 'a
  | Sample of 'a sample          (* Continuous distribution *)
  (* | Discrete of float list    (* list of probabilities; i-th element is probability of float(i) *) *)
  | DistrCase of ('a * float) list (* General discrete distribution: (expr * prob) list *)
  | Cmp    of cmp_op * 'a * 'a   (* Parameterized comparison *)
  | And    of 'a * 'a            
  | Or     of 'a * 'a            
  | Not    of 'a                   
  | If     of 'a * 'a * 'a
  | Pair   of 'a * 'a            (* Pair construction (e1, e2) *)
  | First  of 'a                   (* First projection: fst e *)
  | Second of 'a                   (* Second projection: snd e *)
  | Fun    of string * 'a          (* Function: fun x -> e *)
  | FuncApp    of 'a * 'a            (* Function application: e1 e2 *)
  | LoopApp    of 'a * 'a * int           (* Loop application: e1 e2 int *)
  | FinConst of int * int (* k, n for k#n *)
  | FinCmp of cmp_op * 'a * 'a * int (* Parameterized finite comparison *)
  | FinEq of 'a * 'a * int (* e1 ==#n e2 *)
  | Observe of 'a 
  | Fix of string * string * 'a
  | Nil  (* nil *)
  | Cons of 'a * 'a (* e1 :: e2 *)
  | MatchList of 'a * 'a * string * string * 'a (* match e1 with nil -> e_nil | y::ys -> e_cons end *)
  | Ref of 'a (* ref e *)
  | Deref of 'a (* !e *)
  | Assign of 'a * 'a (* e1 := e2 *)
  | Seq of 'a * 'a (* e1 ; e2 *)
  | Unit
  | RuntimeError of string

and single_arg_dist_kind =
  | DExponential | DLaplace | DCauchy | DTDist | DChi2 | DLogistic | DRayleigh

and two_arg_dist_kind =
  | DUniform | DGaussian | DBeta | DLogNormal | DGamma | DPareto | DWeibull 
  | DGumbel1 | DGumbel2 | DExppow

and 'a sample = 
  | Distr1 of single_arg_dist_kind * 'a
  | Distr2 of two_arg_dist_kind * 'a * 'a
  

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
  | TUnit 
  | TList of ty (* list t *)
  | TRef of ty (* t ref *)

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

(* Function to recursively set all bound bags in float types to top *)
let rec set_bound_bags_to_top (t : ty) : unit =
  match force t with
  | TFloat (bound_bag, _float_bag) ->
      (* Set the bound bag to Top using leq with a Top bag *)
      BoundBag.leq (BoundBag.create Top) bound_bag
  | TPair (t1, t2) ->
      set_bound_bags_to_top t1;
      set_bound_bags_to_top t2
  | TFun (t1, t2) ->
      set_bound_bags_to_top t1;
      set_bound_bags_to_top t2
  | TList t' ->
      set_bound_bags_to_top t'
  | TRef t' ->
      set_bound_bags_to_top t'
  | TMeta r ->
      (* For unresolved type variables, set up a listener to handle future resolution *)
      listen r (fun resolved_t -> set_bound_bags_to_top resolved_t)
  | TBool | TFin _ | TUnit ->
      (* Base types without nested types - nothing to do *)
      ()

(* Typed expressions (recursive definition with aexpr) *)

type texpr = ty * aexpr
and aexpr = TAExprNode of texpr expr_generic

(* Runtime values *)
type value =
  | VBool of bool
  | VFloat of float
  | VPair of value * value
  | VFin of int * int (* value k, modulus n *)
  | VClosure of string * expr * env
  | VUnit 
  | VNil (* Runtime value for nil *)
  | VCons of value * value (* Runtime value for cons *)
  | VRef of value ref (* Runtime value for references *)
and env = (string * value) list (* Simple association list for environment *)

(* Helper for pretty printing values *)
let rec string_of_value = function
  | VBool b -> string_of_bool b
  | VFloat f -> string_of_float f
  | VPair (v1, v2) -> Printf.sprintf "(%s, %s)" (string_of_value v1) (string_of_value v2)
  | VFin (k, n) -> Printf.sprintf "%d#%d" k n
  | VClosure (x, _, _) -> Printf.sprintf "<fun %s>" x
  | VUnit -> "()"
  | VNil -> "[]"
  | VCons (v_hd, VNil) -> Printf.sprintf "[%s]" (string_of_value v_hd) (* Special case for single-element list *)
  | VCons (v_hd, v_tl) -> Printf.sprintf "%s :: %s" (string_of_value v_hd) (string_of_value v_tl) (* General cons - avoid infinite loop by not trying to fully format *)
  | VRef v -> Printf.sprintf "ref(%s)" (string_of_value !v) (* Show current value *) 