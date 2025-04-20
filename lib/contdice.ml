(* Main implementation of continuous dice *)

open Types
open Bags (* Open Bags to access FloatSet and FloatBag *)
(* FloatBag is now defined in Bags module *)

(* Re-export internal modules needed by executable/tests *)
module Parse = Parse 
module Pretty = Pretty
(* We might need others like Types, Bag, Stats depending on usage, 
   but let's start with these two. *)

module StringMap = Map.Make(String)

(* ======== Types and unification ======== *)

let rec force t =
  match t with
  | TMeta r ->
      (match !r with
      | Some t -> force t
      | None -> t)
  | _ -> t

let rec unify (t1 : ty) (t2 : ty) : unit =
  match force t1, force t2 with
  | TBool,    TBool      -> ()
  | TFloat b1, TFloat b2 -> Bags.FloatBag.union b1 b2
  | TInt,     TInt       -> ()
  | TPair(a1, b1), TPair(a2, b2) -> 
      unify a1 a2; 
      unify b1 b2
  | TFun(a1, b1), TFun(a2, b2) -> 
      unify a1 a2; 
      unify b1 b2
  | TBool,    TFloat _   -> failwith "Type error: expected bool, got float"
  | TBool,    TInt       -> failwith "Type error: expected bool, got int"
  | TBool,    TPair _    -> failwith "Type error: expected bool, got pair"
  | TBool,    TFun _     -> failwith "Type error: expected bool, got function"
  | TFloat _, TBool      -> failwith "Type error: expected float, got bool"
  | TFloat _, TInt       -> failwith "Type error: expected float, got int"
  | TFloat _, TPair _    -> failwith "Type error: expected float, got pair"
  | TFloat _, TFun _     -> failwith "Type error: expected float, got function"
  | TInt,     TBool      -> failwith "Type error: expected int, got bool"
  | TInt,     TFloat _   -> failwith "Type error: expected int, got float"
  | TInt,     TPair _    -> failwith "Type error: expected int, got pair"
  | TInt,     TFun _     -> failwith "Type error: expected int, got function"
  | TPair _,  TBool      -> failwith "Type error: expected pair, got bool"
  | TPair _,  TFloat _   -> failwith "Type error: expected pair, got float"
  | TPair _,  TInt       -> failwith "Type error: expected pair, got int"
  | TPair _,  TFun _     -> failwith "Type error: expected pair, got function"
  | TFun _,   TBool      -> failwith "Type error: expected function, got bool"
  | TFun _,   TFloat _   -> failwith "Type error: expected function, got float"
  | TFun _,   TInt       -> failwith "Type error: expected function, got int"
  | TFun _,   TPair _    -> failwith "Type error: expected function, got pair"
  | TMeta r1, _   ->
      r1 := Some t2
  | _, TMeta r2   ->
      r2 := Some t1

(* ======== Annotated expressions ======== *)

(* Elaborator: expr -> (ty * aexpr), generating bag constraints *)
let elab (e : expr) : texpr =
  let rec aux (env : ty StringMap.t) (ExprNode e_node : expr) : texpr =
    match e_node with
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
      let b = Bags.FloatBag.create (Finite FloatSet.empty) in
      (TFloat b, TAExprNode (CDistr dist))
      
    | Discrete probs ->
      let sum = List.fold_left (+.) 0.0 probs in
      if abs_float (sum -. 1.0) > 0.0001 then
        failwith (Printf.sprintf "Discrete distribution probabilities must sum to 1.0, got %f" sum);
      (TInt, TAExprNode (Discrete probs))

    | Less (e1, f) ->
      let t1, a1 = aux env e1 in
      let b = Bags.FloatBag.create (Finite FloatSet.empty) in
      unify t1 (TFloat b);
      let singleton_bag = Bags.FloatBag.create (Finite (FloatSet.singleton f)) in
      Bags.FloatBag.union b singleton_bag;
      (TBool, TAExprNode (Less ((t1,a1), f)))
      
    | LessEq (e1, n) ->
      let t1, a1 = aux env e1 in
      unify t1 TInt;
      (TBool, TAExprNode (LessEq ((t1,a1), n)))

    | If (e1, e2, e3) ->
      let t1, a1 = aux env e1 in
      unify t1 TBool;
      let t2, a2 = aux env e2 in
      let t3, a3 = aux env e3 in
      unify t2 t3;
      (t2, TAExprNode (If ((t1,a1), (t2,a2), (t3,a3))))
      
    | Pair (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      (TPair (t1, t2), TAExprNode (Pair ((t1, a1), (t2, a2))))
      
    | First e1 ->
      let t, a = aux env e1 in
      let t1 = TMeta (ref None) in
      let t2 = TMeta (ref None) in
      unify t (TPair (t1, t2));
      (t1, TAExprNode (First (t, a)))
      
    | Second e1 ->
      let t, a = aux env e1 in
      let t1 = TMeta (ref None) in
      let t2 = TMeta (ref None) in
      unify t (TPair (t1, t2));
      (t2, TAExprNode (Second (t, a)))
      
    | Fun (x, e1) ->
      let param_type = TMeta (ref None) in
      let env' = StringMap.add x param_type env in
      let return_type, a = aux env' e1 in
      (TFun (param_type, return_type), TAExprNode (Fun (x, (return_type, a))))
      
    | App (e1, e2) ->
      let t1, a1 = aux env e1 in
      let t2, a2 = aux env e2 in
      let result_type = TMeta (ref None) in
      unify t1 (TFun (t2, result_type));
      (result_type, TAExprNode (App ((t1, a1), (t2, a2))))
  in

  aux StringMap.empty e

(* Function that does elab but insists that the return type is TBool *)
let elab_bool (e : expr) : texpr =
  let t, a = elab e in
  match force t with
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
    | Var x ->
        ExprNode (Var x)

    | Let (x, te1, te2) ->
        ExprNode (Let (x, aux te1, aux te2))

    | CDistr dist ->
        let b =
          match ty with TFloat b -> b | _ -> failwith "Internal error: CDistr not TFloat"
        in
        (* Get the set or top associated with the bag *) 
        let set_or_top_val = Bags.FloatBag.get b in 
        let cuts = 
          match set_or_top_val with
          | Top -> failwith "Cannot discretize a distribution compared with Top boundary" 
          | Finite float_set -> FloatSet.elements float_set 
        in 
        let intervals = List.init (List.length cuts + 1) (fun i ->
          let left = if i = 0 then neg_infinity else List.nth cuts (i - 1) in
          let right = if i = List.length cuts then infinity else List.nth cuts i in
          (left, right)
        ) in
        let probs = List.map (fun (left, right) ->
          prob_cdistr_interval left right dist
        ) intervals in
        ExprNode (Discrete probs)
        
    | Discrete probs ->
        ExprNode (Discrete probs)

    | Less ((t_sub, _) as te_sub, f) ->
        let d_sub = aux te_sub in
        let cuts =
          match force t_sub with 
          | TFloat b -> 
              let set_or_top_val = Bags.FloatBag.get b in (* Get the set or top *)
              (match set_or_top_val with
               | Top -> failwith "Cannot perform Less comparison with Top boundary"
               | Finite float_set -> FloatSet.elements float_set)
          | _ -> failwith "Type error: Less expects float"
        in
        let idx = List.length (List.filter (fun x -> x < f) cuts) in
        ExprNode (LessEq (d_sub, idx))
        
    | LessEq ((_, _) as te_sub, n) ->
        let d_sub = aux te_sub in
        ExprNode (LessEq (d_sub, n))

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
  in
  aux e