(* Main implementation of continuous dice *)

open Types
open Bags (* Open Bags to access FloatSet, BoundSet, FloatBag, BoundBag etc. *)
(* FloatBag is now defined in Bags module *)

(* Re-export internal modules needed by executable/tests *)
module Parse = Parse 
module Pretty = Pretty
(* We might need others like Types, Bag, Stats depending on usage, 
   but let's start with these two. *)

module StringMap = Map.Make(String)

(* ======== Types and unification ======== *)

let rec unify (t1 : ty) (t2 : ty) : unit =
  match Types.force t1, Types.force t2 with
  | TBool,    TBool      -> ()
  | TFloat (b1, c1), TFloat (b2, c2) -> 
      Bags.BoundBag.union b1 b2;
      Bags.FloatBag.union c1 c2
  | TPair(a1, b1), TPair(a2, b2) -> 
      unify a1 a2; 
      unify b1 b2
  | TFun(a1, b1), TFun(a2, b2) -> 
      unify a1 a2; 
      unify b1 b2
  | TFin n1, TFin n2 when n1 = n2 -> () (* Unify TFin n only if n matches *)
  | TMeta _, t2 when t1 == t2 -> () (* Avoid circular unification (ignore ref) *)
  | t1, TMeta _ when t1 == t2 -> () (* Avoid circular unification (ignore ref) *)
  | TMeta r1, _   ->
      (match !r1 with
      | Some t1' -> unify t1' t2
      | None -> r1 := Some t2)
  | _, TMeta r2   ->
      (match !r2 with
      | Some t2' -> unify t1 t2'
      | None -> r2 := Some t1)
  | _, _ -> 
      (* Improved error reporting *)
      let msg = Printf.sprintf "Type mismatch: cannot unify %s and %s"
        (Pretty.string_of_ty t1) (Pretty.string_of_ty t2)
      in
      failwith msg

(* ======== Annotated expressions ======== *)

(* Elaborator: expr -> texpr, generating bag constraints and performing type checking *)
let elab (e : expr) : texpr =
  let rec aux (env : ty StringMap.t) (ExprNode e_node : expr) : texpr =
    match e_node with
    | Const f -> 
      (* Constant float: Create bag refs *) 
      let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in 
      let consts_bag_ref = Bags.FloatBag.create (Finite (FloatSet.singleton f)) in
      (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (Const f))
    | Var x ->
      (try 
        let ty = StringMap.find x env in
        (ty, TAExprNode (Var x))
       with Not_found -> 
        failwith ("Unbound variable: " ^ x))

    | Let (x, e1, e2) ->
      let t1, a1 = aux env e1 in
      let env' = StringMap.add x t1 env in
      let t2, a2 = aux env' e2 in
      (t2, TAExprNode (Let (x, (t1,a1), (t2,a2))))

    | CDistr dist ->
      (* CDistr results in TFloat: Create bag refs *) 
      let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in 
      let consts_bag_ref = Bags.FloatBag.create Top in 
      (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (CDistr dist))
      
    | Discrete probs ->
      let sum = List.fold_left (+.) 0.0 probs in
      if abs_float (sum -. 1.0) > 0.0001 then
        failwith (Printf.sprintf "Discrete distribution probabilities must sum to 1.0, got %f" sum);
      (* Discrete results in TFloat: Create bag refs *) 
      let bounds_bag_ref = Bags.BoundBag.create (Finite BoundSet.empty) in 
      let const_values = List.init (List.length probs) float_of_int |> FloatSet.of_list in
      let consts_bag_ref = Bags.FloatBag.create (Finite const_values) in
      (TFloat (bounds_bag_ref, consts_bag_ref), TAExprNode (Discrete probs))

    | Less (e1, f) ->
      let t1, a1 = aux env e1 in
      (* Create fresh TFloat refs for unification (bounds only contain the compared value) *)
      let fresh_bounds_bag_ref = Bags.BoundBag.create (Finite (BoundSet.singleton (Bags.Less f))) in
      let fresh_consts_bag_ref = Bags.FloatBag.create (Finite FloatSet.empty) in
      let target_type = TFloat (fresh_bounds_bag_ref, fresh_consts_bag_ref) in
      (try unify t1 target_type
       with Failure msg -> failwith (Printf.sprintf "Type error in Less (< %g): %s" f msg));
      (* Add the Less f bound by creating a temporary bag and unioning *)
      (match Types.force t1 with
       | TFloat (b_ref, _) -> 
           let new_bound_bag = Bags.BoundBag.create (Finite (BoundSet.singleton (Bags.Less f))) in
           Bags.BoundBag.union b_ref new_bound_bag
       | _ -> failwith "Internal error: Less operand not TFloat after unification");
      (TBool, TAExprNode (Less ((t1,a1), f)))
      
    | LessEq (e1, f) ->
      let t1, a1 = aux env e1 in
      (* Create fresh TFloat refs for unification (bounds only contain the compared value) *)
      let fresh_bounds_bag_ref = Bags.BoundBag.create (Finite (BoundSet.singleton (Bags.LessEq f))) in
      let fresh_consts_bag_ref = Bags.FloatBag.create (Finite FloatSet.empty) in
      let target_type = TFloat (fresh_bounds_bag_ref, fresh_consts_bag_ref) in
       (try unify t1 target_type
       with Failure msg -> failwith (Printf.sprintf "Type error in LessEq (<= %g): %s" f msg));
      (* Add the LessEq f bound by creating a temporary bag and unioning *)
      (match Types.force t1 with
       | TFloat (b_ref, _) -> 
           let new_bound_bag = Bags.BoundBag.create (Finite (BoundSet.singleton (Bags.LessEq f))) in
           Bags.BoundBag.union b_ref new_bound_bag
       | _ -> failwith "Internal error: LessEq operand not TFloat after unification");
      (TBool, TAExprNode (LessEq ((t1,a1), f)))

    | If (e1, e2, e3) ->
      let t1, a1 = aux env e1 in
      (try unify t1 TBool 
       with Failure msg -> failwith ("Type error in If condition: " ^ msg));
      let t2, a2 = aux env e2 in
      let t3, a3 = aux env e3 in
      (try unify t2 t3 
       with Failure msg -> failwith ("Type error in If branches: " ^ msg)); 
      (t2, TAExprNode (If ((t1,a1), (t2,a2), (t3,a3))))
      
    | Pair (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (TPair (t1, t2), TAExprNode (Pair ((t1, a1), (t2, a2))))
      
    | First e1 ->
      let t, a = aux env e1 in
      let t1_meta = TMeta (ref None) in
      let t2_meta = TMeta (ref None) in
      (try unify t (TPair (t1_meta, t2_meta))
       with Failure msg -> failwith ("Type error in First (fst): " ^ msg));
      (Types.force t1_meta, TAExprNode (First (t, a))) (* Use Types.force *)
      
    | Second e1 ->
      let t, a = aux env e1 in
      let t1_meta = TMeta (ref None) in
      let t2_meta = TMeta (ref None) in
      (try unify t (TPair (t1_meta, t2_meta))
       with Failure msg -> failwith ("Type error in Second (snd): " ^ msg));
      (Types.force t2_meta, TAExprNode (Second (t, a))) (* Use Types.force *)
      
    | Fun (x, e1) ->
      let param_type = TMeta (ref None) in
      let env' = StringMap.add x param_type env in
      let return_type, a = aux env' e1 in
      (TFun (param_type, return_type), TAExprNode (Fun (x, (return_type, a))))
      
    | App (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let result_type = TMeta (ref None) in
      (try unify t1 (TFun (t2, result_type))
        with Failure msg -> failwith ("Type error in function application: " ^ msg));
      (Types.force result_type, TAExprNode (App ((t1, a1), (t2, a2)))) (* Use Types.force *)
      
    | FinConst (k, n) ->
      if k < 0 || k >= n then failwith (Printf.sprintf "Invalid FinConst value: %d#%d. k must be >= 0 and < n." k n);
      (TFin n, TAExprNode (FinConst (k, n)))

    | FinLt (e1, e2, n) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinLt modulus: <#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = TFin n in
      (try unify t1 expected_type 
       with Failure msg -> failwith (Printf.sprintf "Type error in FinLt (<#%d) left operand: %s" n msg));
      (try unify t2 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinLt (<#%d) right operand: %s" n msg));
      (TBool, TAExprNode (FinLt ((t1, a1), (t2, a2), n)))
      
    | FinLeq (e1, e2, n) ->
      if n <= 0 then failwith (Printf.sprintf "Invalid FinLeq modulus: <=#%d. n must be > 0." n);
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let expected_type = TFin n in
      (try unify t1 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinLeq (<=#%d) left operand: %s" n msg));
      (try unify t2 expected_type
       with Failure msg -> failwith (Printf.sprintf "Type error in FinLeq (<=#%d) right operand: %s" n msg));
      (TBool, TAExprNode (FinLeq ((t1, a1), (t2, a2), n)))

  in
  aux StringMap.empty e

(* Function that does elab but insists that the return type is TBool *)
let elab_bool (e : expr) : texpr =
  let t, a = elab e in
  match Types.force t with
  | TBool -> (t, a)
  | _ -> failwith "Type error: expected bool, got something else"

(* Calculate probability for a given distribution in an interval *)
let prob_cdistr_interval (left : float) (right : float) (dist : Stats.cdistr) : float =
  let cdf = Stats.cdistr_cdf dist in
  cdf right -. cdf left

(* 
Discretizer from typed expressions to discrete expressions.

The idea is that the type system infers a bag of floats that each 
expression possibly compares against. Instead of sampling from a continuous 
distribution, we sample from a discrete distribution that tells us the 
probabilities of the interval between two floats.

When doing a comparison against a float, we convert that to a comparison 
against the discrete integer that represents the i-th float in the bag.
*)
let discretize (e : texpr) : expr =
  let rec aux ((ty, TAExprNode ae_node) : texpr) : expr =
    match ae_node with
    | Const f -> 
        ExprNode (Const f) (* Pass constant through *) 
    | Var x ->
        ExprNode (Var x)

    | Let (x, te1, te2) ->
        ExprNode (Let (x, aux te1, aux te2))

    | CDistr dist ->
        let bounds_bag =
          match Types.force ty with 
          | TFloat (b, _) -> b (* Extract bounds bag *)
          | _ -> failwith "Internal error: CDistr not TFloat"
        in
        let set_or_top_val = Bags.BoundBag.get bounds_bag in 
        (match set_or_top_val with
         | Top -> ExprNode (CDistr dist) (* Keep original if Top *) 
         | Finite bound_set -> 
             (* Discretize using cuts *) 
             let cuts = 
               BoundSet.elements bound_set 
               |> List.map (function Bags.Less c -> c | Bags.LessEq c -> c) 
               |> List.sort_uniq compare
             in 
             let intervals = List.init (List.length cuts + 1) (fun i ->
               let left = if i = 0 then neg_infinity else List.nth cuts (i - 1) in
               let right = if i = List.length cuts then infinity else List.nth cuts i in
               (left, right)
             ) in
             let probs = List.map (fun (left, right) ->
               prob_cdistr_interval left right dist
             ) intervals in
             ExprNode (Discrete probs))
        
    | Discrete probs ->
        ExprNode (Discrete probs)

    | Less ((t_sub, _) as te_sub, f) ->
        let d_sub = aux te_sub in
        (match Types.force t_sub with 
         | TFloat (bounds_bag, _) -> (* Extract bounds bag *) 
             let set_or_top_val = Bags.BoundBag.get bounds_bag in
             (match set_or_top_val with
              | Top -> ExprNode (Less (d_sub, f)) (* Keep original Less if Top *) 
              | Finite bound_set -> 
                  (* Discretize using cuts *) 
                  let cuts = 
                    BoundSet.elements bound_set 
                    |> List.map (function Bags.Less c -> c | Bags.LessEq c -> c)
                    |> List.sort_uniq compare
                  in
                  let idx = List.length (List.filter (fun x -> x < f) cuts) in
                  ExprNode (LessEq (d_sub, float_of_int idx)))
         | _ -> failwith "Type error: Less expects float")
        
    | LessEq ((t_sub, _) as te_sub, f) -> 
        let d_sub = aux te_sub in
        (match Types.force t_sub with 
         | TFloat (bounds_bag, _) -> (* Extract bounds bag *) 
             let set_or_top_val = Bags.BoundBag.get bounds_bag in
             (match set_or_top_val with
              | Top -> ExprNode (LessEq (d_sub, f)) (* Keep original LessEq if Top *) 
              | Finite bound_set -> 
                  (* Discretize using cuts *) 
                  let cuts = 
                    BoundSet.elements bound_set 
                    |> List.map (function Bags.Less c -> c | Bags.LessEq c -> c)
                    |> List.sort_uniq compare
                  in
                  let idx = List.length (List.filter (fun x -> x <= f) cuts) in
                  ExprNode (LessEq (d_sub, float_of_int idx)))
         | _ -> failwith "Type error: LessEq expects float")

    | If (te1, te2, te3) ->
        ExprNode (If (aux te1, aux te2, aux te3))
        
    | Pair (te1, te2) ->
        ExprNode (Pair (aux te1, aux te2))
        
    | First te ->
        ExprNode (First (aux te))
        
    | Second te ->
        ExprNode (Second (aux te))
        
    | Fun (x, te) ->
        ExprNode (Fun (x, aux te))
        
    | App (te1, te2) ->
        ExprNode (App (aux te1, aux te2))

    (* Fin types are already discrete, pass them through *) 
    | FinConst (k, n) -> 
        ExprNode (FinConst (k, n))
    | FinLt (te1, te2, n) -> 
        ExprNode (FinLt (aux te1, aux te2, n))
    | FinLeq (te1, te2, n) -> 
        ExprNode (FinLeq (aux te1, aux te2, n))

  in
  aux e