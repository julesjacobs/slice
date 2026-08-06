(* Type definitions for Slice *)

type cmp_op = Lt | Le

(* Generic expression structure *)
type 'a expr_generic = 
  | Var    of string
  | Const  of float
  | BoolConst of bool            
  | Let    of string * 'a * 'a
  | Sample of 'a sample
  | DistrCase of ('a * float) list
  | Cmp    of cmp_op * 'a * 'a * bool
  | And    of 'a * 'a            
  | Or     of 'a * 'a            
  | Not    of 'a                   
  | If     of 'a * 'a * 'a
  | Pair   of 'a * 'a
  | First  of 'a
  | Second of 'a
  | Fun    of string * 'a
  | FuncApp    of 'a * 'a
  | LoopApp    of 'a * 'a * int           (* Loop application: e1 e2 int *)
  | FinConst of int * int
  | FinCmp of cmp_op * 'a * 'a * int * bool
  | FinEq of 'a * 'a * int
  | Observe of 'a 
  | Fix of string * string * 'a
  | Nil
  | Cons of 'a * 'a
  | MatchList of 'a * 'a * string * string * 'a
  | Ref of 'a
  | Deref of 'a
  | Assign of 'a * 'a
  | Seq of 'a * 'a
  | Unit
  | RuntimeError of string

and single_arg_dist_kind =
  | DExponential | DLaplace | DCauchy | DTDist | DChi2 | DLogistic | DRayleigh | DPoisson

and two_arg_dist_kind =
  | DUniform | DGaussian | DBeta | DLogNormal | DGamma | DPareto | DWeibull 
  | DGumbel1 | DGumbel2 | DExppow | DBinomial

and 'a sample = 
  | Distr1 of single_arg_dist_kind * 'a
  | Distr2 of two_arg_dist_kind * 'a * 'a
  

type expr = ExprNode of expr expr_generic

type probability_policy =
  | Generated
  | Rounded_input

let probability_sum_tolerance policy count =
  match policy with
  | Generated -> 1e-10 *. float_of_int (max 1 count)
  | Rounded_input -> 1e-4

let normalize_probability_weights ?(policy = Generated)
    (weights : float list) : float list =
  if weights = [] then
    failwith "A discrete distribution must have at least one outcome";
  List.iter
    (fun p ->
      match classify_float p with
      | FP_nan | FP_infinite ->
          failwith "Discrete distribution weights must be finite"
      | FP_normal | FP_subnormal | FP_zero ->
          if p < 0.0 then
            failwith "Discrete distribution weights must be nonnegative")
    weights;
  let sum = List.fold_left (+.) 0.0 weights in
  let tolerance = probability_sum_tolerance policy (List.length weights) in
  if classify_float sum = FP_infinite
     || abs_float (sum -. 1.0) > tolerance
  then
    failwith
      (Printf.sprintf
         "Discrete distribution weights must sum to 1.0, got %.17g" sum);
  (* Generated probabilities differ from one only through floating-point CDF
     arithmetic. Rounded source literals get the documented 1e-4 input policy.
     In both cases, preserve all normalized prefixes and make the final weight
     their binary64 complement so cumulative sampling has an exact endpoint. *)
  let normalized = List.map (fun p -> p /. sum) weights in
  let rec close_prefix prefix_sum = function
    | [] -> assert false
    | [_] ->
        let final = 1.0 -. prefix_sum in
        if final < 0.0 then
          failwith "Discrete distribution normalization produced a negative final weight"
        else [final]
    | p :: rest -> p :: close_prefix (prefix_sum +. p) rest
  in
  close_prefix 0.0 normalized

let normalize_distr_case ?(policy = Generated)
    (cases : ('a * float) list) : ('a * float) list =
  let weights = normalize_probability_weights ~policy (List.map snd cases) in
  List.map2 (fun (e, _) p -> (e, p)) cases weights

let distr_case ?(policy = Generated) (cases : (expr * float) list) : expr =
  ExprNode (DistrCase (normalize_distr_case ~policy cases))

open Lats

type meta =
  | Unknown of (ty -> unit) list
  | Known of ty
and meta_ref = meta ref
and ty =
  | TBool
  | TFloat of CutLat.bag * FloatLat.bag (* Store bag REFERENCES, not contents *)
  | TPair of ty * ty
  | TFun of ty * ty
  | TFin of int
  | TMeta of meta_ref
  | TUnit 
  | TList of ty
  | TRef of ty

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
let rec set_cut_bags_to_top (t : ty) : unit =
  match force t with
  | TFloat (cut_bag, _float_bag) ->
      (* Set the bound bag to Top using leq with a Top bag *)
      CutLat.leq (CutLat.create Top) cut_bag
  | TPair (t1, t2) ->
      set_cut_bags_to_top t1;
      set_cut_bags_to_top t2
  | TFun (t1, t2) ->
      set_cut_bags_to_top t1;
      set_cut_bags_to_top t2
  | TList t' ->
      set_cut_bags_to_top t'
  | TRef t' ->
      set_cut_bags_to_top t'
  | TMeta r ->
      (* For unresolved type variables, set up a listener to handle future resolution *)
      listen r (fun resolved_t -> set_cut_bags_to_top resolved_t)
  | TBool | TFin _ | TUnit ->
      (* Base types without nested types - nothing to do *)
      ()

type texpr = ty * aexpr
and aexpr = TAExprNode of texpr expr_generic

type value =
  | VBool of bool
  | VFloat of float
  | VPair of value * value
  | VFin of int * int (* value k, modulus n *)
  | VClosure of string * expr * env
  | VUnit 
  | VNil
  | VCons of value * value
  | VRef of value ref
and env = (string * value) list

let rec string_of_value = function
  | VBool b -> string_of_bool b
  | VFloat f -> string_of_float f
  | VPair (v1, v2) -> Printf.sprintf "(%s, %s)" (string_of_value v1) (string_of_value v2)
  | VFin (k, n) -> Printf.sprintf "%d#%d" k n
  | VClosure (x, _, _) -> Printf.sprintf "<fun %s>" x
  | VUnit -> "()"
  | VNil -> "[]"
  | VCons (v_hd, VNil) -> Printf.sprintf "[%s]" (string_of_value v_hd)
  | VCons (v_hd, v_tl) -> Printf.sprintf "%s :: %s" (string_of_value v_hd) (string_of_value v_tl)
  | VRef v -> Printf.sprintf "ref(%s)" (string_of_value !v)
