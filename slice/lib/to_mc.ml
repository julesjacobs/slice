module A = Ast

(* Constructs a Markov chain based off the small-step operational semantics of Slice. *)

type transition = {
  src : int;
  dst : int;
  prob : float;
}

type mc = {
  num_states : int;
  sink : int;
  transitions : transition list;
  expr_of : (int, A.expr) Hashtbl.t; (* map: state id -> expression *)
  has_observe : bool;
  fail : int option;
  error : int option;
}

(* ---------- Utilities: sets of identifiers ---------- *)

module StringSet = Set.Make (String)

let rec free_vars (A.ExprNode e : A.expr) : StringSet.t =
  match e with
  | A.Var x -> StringSet.singleton x
  | A.Const _ | A.BoolConst _ | A.FinConst _ | A.Unit | A.Nil | A.RuntimeError _ -> StringSet.empty
  | A.Let (x, e1, e2) ->
      StringSet.union (free_vars e1) (StringSet.remove x (free_vars e2))
  | A.Sample s ->
      (match s with
       | A.Distr1 (_, e1) -> free_vars e1
       | A.Distr2 (_, e1, e2) -> StringSet.union (free_vars e1) (free_vars e2))
  | A.DistrCase cases ->
      List.fold_left (fun acc (ei, _p) -> StringSet.union acc (free_vars ei)) StringSet.empty cases
  | A.Cmp (_, e1, e2, _)
  | A.And (e1, e2)
  | A.Or (e1, e2)
  | A.Pair (e1, e2)
  | A.Cons (e1, e2)
  | A.FuncApp (e1, e2)
  | A.LoopApp (e1, e2, _)
  | A.FinEq (e1, e2, _)
  | A.Assign (e1, e2)
  | A.Seq (e1, e2) ->
      StringSet.union (free_vars e1) (free_vars e2)
  | A.Not e1
  | A.First e1
  | A.Second e1
  | A.Deref e1
  | A.Ref e1
  | A.Observe e1 ->
      free_vars e1
  | A.If (c, t, f) ->
      StringSet.union (free_vars c) (StringSet.union (free_vars t) (free_vars f))
  | A.FinCmp (_cmp, e1, e2, _n, _) ->
      StringSet.union (free_vars e1) (free_vars e2)
  | A.Fun (x, body) ->
      StringSet.remove x (free_vars body)
  | A.Fix (f, x, body) ->
      free_vars body |> StringSet.remove f |> StringSet.remove x
  | A.MatchList (e, nil_br, x, xs, cons_br) ->
      StringSet.union (free_vars e)
        (StringSet.union (free_vars nil_br)
           (free_vars cons_br |> StringSet.remove x |> StringSet.remove xs))

let rec contains_runtime_error (A.ExprNode e : A.expr) : bool =
  let either e1 e2 = contains_runtime_error e1 || contains_runtime_error e2 in
  match e with
  | A.RuntimeError _ -> true
  | A.Var _ | A.Const _ | A.BoolConst _ | A.FinConst _ | A.Unit | A.Nil -> false
  | A.Let (_, e1, e2) | A.Cmp (_, e1, e2, _) | A.And (e1, e2)
  | A.Or (e1, e2) | A.Pair (e1, e2) | A.FuncApp (e1, e2)
  | A.Cons (e1, e2) | A.Assign (e1, e2) | A.Seq (e1, e2)
  | A.FinEq (e1, e2, _) | A.LoopApp (e1, e2, _) ->
      either e1 e2
  | A.Sample (A.Distr1 (_, e1)) | A.Not e1 | A.First e1 | A.Second e1
  | A.Observe e1 | A.Ref e1 | A.Deref e1 ->
      contains_runtime_error e1
  | A.Fun _ | A.Fix _ -> false
  | A.Sample (A.Distr2 (_, e1, e2)) | A.FinCmp (_, e1, e2, _, _) ->
      either e1 e2
  | A.DistrCase cases ->
      List.exists (fun (case, _) -> contains_runtime_error case) cases
  | A.If (e1, e2, e3) ->
      contains_runtime_error e1 || contains_runtime_error e2
      || contains_runtime_error e3
  | A.MatchList (e1, e2, _, _, e3) ->
      contains_runtime_error e1 || contains_runtime_error e2
      || contains_runtime_error e3

let fresh_name (avoid : StringSet.t) (base : string) : string =
  if not (StringSet.mem base avoid) then base
  else
    let rec go i =
      let cand = base ^ "'" ^ string_of_int i in
      if StringSet.mem cand avoid then go (i + 1) else cand
    in
    go 0

(* Capture-avoiding renaming: replace bound var [x] with [x'] in [body]. *)
let rec rename (x : string) (x' : string) (A.ExprNode e : A.expr) : A.expr =
  match e with
  | A.Var y -> if y = x then A.ExprNode (A.Var x') else A.ExprNode e
  | A.Const _ | A.BoolConst _ | A.FinConst _ | A.Unit | A.Nil | A.RuntimeError _ ->
      A.ExprNode e
  | A.Let (y, e1, e2) ->
      let e1' = rename x x' e1 in
      if y = x then A.ExprNode (A.Let (x', e1', rename x x' e2))
      else A.ExprNode (A.Let (y, e1', rename x x' e2))
  | A.Sample s ->
      (match s with
       | A.Distr1 (k, e1) -> A.ExprNode (A.Sample (A.Distr1 (k, rename x x' e1)))
       | A.Distr2 (k, e1, e2) ->
           A.ExprNode (A.Sample (A.Distr2 (k, rename x x' e1, rename x x' e2))))
  | A.DistrCase cases ->
      A.ExprNode (A.DistrCase (List.map (fun (ei, p) -> (rename x x' ei, p)) cases))
  | A.Cmp (op, e1, e2, flipped) ->
      A.ExprNode (A.Cmp (op, rename x x' e1, rename x x' e2, flipped))
  | A.FinCmp (op, e1, e2, n, flipped) ->
      A.ExprNode (A.FinCmp (op, rename x x' e1, rename x x' e2, n, flipped))
  | A.FinEq (e1, e2, n) ->
      A.ExprNode (A.FinEq (rename x x' e1, rename x x' e2, n))
  | A.And (e1, e2) -> A.ExprNode (A.And (rename x x' e1, rename x x' e2))
  | A.Or (e1, e2) -> A.ExprNode (A.Or (rename x x' e1, rename x x' e2))
  | A.Not e1 -> A.ExprNode (A.Not (rename x x' e1))
  | A.If (c, t, f) -> A.ExprNode (A.If (rename x x' c, rename x x' t, rename x x' f))
  | A.Pair (e1, e2) -> A.ExprNode (A.Pair (rename x x' e1, rename x x' e2))
  | A.First e1 -> A.ExprNode (A.First (rename x x' e1))
  | A.Second e1 -> A.ExprNode (A.Second (rename x x' e1))
  | A.Fun (y, body) ->
      if y = x then A.ExprNode (A.Fun (x', rename x x' body))
      else A.ExprNode (A.Fun (y, rename x x' body))
  | A.FuncApp (e1, e2) -> A.ExprNode (A.FuncApp (rename x x' e1, rename x x' e2))
  | A.LoopApp (e1, e2, n) -> A.ExprNode (A.LoopApp (rename x x' e1, rename x x' e2, n))
  | A.Observe e1 -> A.ExprNode (A.Observe (rename x x' e1))
  | A.Fix (f, y, body) ->
      let f' = if f = x then x' else f in
      let y' = if y = x then x' else y in
      A.ExprNode (A.Fix (f', y', rename x x' body))
  | A.Cons (hd, tl) -> A.ExprNode (A.Cons (rename x x' hd, rename x x' tl))
  | A.MatchList (scrut, nil_br, xh, xt, cons_br) ->
      let scrut' = rename x x' scrut in
      let nil_br' = rename x x' nil_br in
      let xh' = if xh = x then x' else xh in
      let xt' = if xt = x then x' else xt in
      let cons_br' = rename x x' cons_br in
      A.ExprNode (A.MatchList (scrut', nil_br', xh', xt', cons_br'))
  | A.Ref e1 -> A.ExprNode (A.Ref (rename x x' e1))
  | A.Deref e1 -> A.ExprNode (A.Deref (rename x x' e1))
  | A.Assign (e1, e2) -> A.ExprNode (A.Assign (rename x x' e1, rename x x' e2))
  | A.Seq (e1, e2) -> A.ExprNode (A.Seq (rename x x' e1, rename x x' e2))

(* Capture-avoiding substitution [e[v/x]] *)
let rec subst (x : string) (v : A.expr) (A.ExprNode e : A.expr) : A.expr =
  match e with
  | A.Var y -> if y = x then v else A.ExprNode e
  | A.Const _ | A.BoolConst _ | A.FinConst _ | A.Unit | A.Nil | A.RuntimeError _ ->
      A.ExprNode e
  | A.Fun (y, body) ->
      if y = x then A.ExprNode e
      else if StringSet.mem y (free_vars v) then (
        let avoid = StringSet.union (free_vars body) (free_vars v) |> StringSet.add x in
        let y' = fresh_name avoid y in
        let body' = rename y y' body in
        A.ExprNode (A.Fun (y', subst x v body')))
      else A.ExprNode (A.Fun (y, subst x v body))
  | A.Fix (f, y, body) ->
      if f = x || y = x then A.ExprNode e
      else
        let fv_v = free_vars v in
        let body =
          if StringSet.mem f fv_v then (
            let avoid = StringSet.union (free_vars body) fv_v |> StringSet.add x in
            let f' = fresh_name avoid f in
            rename f f' body)
          else body
        in
        let body =
          if StringSet.mem y fv_v then (
            let avoid = StringSet.union (free_vars body) fv_v |> StringSet.add x in
            let y' = fresh_name avoid y in
            rename y y' body)
          else body
        in
        A.ExprNode (A.Fix (f, y, subst x v body))
  | A.Let (y, e1, e2) ->
      let e1' = subst x v e1 in
      if y = x then A.ExprNode (A.Let (y, e1', e2))
      else if StringSet.mem y (free_vars v) then (
        let avoid = StringSet.union (free_vars e2) (free_vars v) |> StringSet.add x in
        let y' = fresh_name avoid y in
        let e2' = rename y y' e2 in
        A.ExprNode (A.Let (y', e1', subst x v e2')))
      else A.ExprNode (A.Let (y, e1', subst x v e2))
  | A.Sample s ->
      (match s with
       | A.Distr1 (k, e1) -> A.ExprNode (A.Sample (A.Distr1 (k, subst x v e1)))
       | A.Distr2 (k, e1, e2) ->
           A.ExprNode (A.Sample (A.Distr2 (k, subst x v e1, subst x v e2))))
  | A.DistrCase cases ->
      A.ExprNode (A.DistrCase (List.map (fun (ei, p) -> (subst x v ei, p)) cases))
  | A.Cmp (op, e1, e2, flipped) ->
      A.ExprNode (A.Cmp (op, subst x v e1, subst x v e2, flipped))
  | A.FinCmp (op, e1, e2, n, flipped) ->
      A.ExprNode (A.FinCmp (op, subst x v e1, subst x v e2, n, flipped))
  | A.FinEq (e1, e2, n) ->
      A.ExprNode (A.FinEq (subst x v e1, subst x v e2, n))
  | A.And (e1, e2) -> A.ExprNode (A.And (subst x v e1, subst x v e2))
  | A.Or (e1, e2) -> A.ExprNode (A.Or (subst x v e1, subst x v e2))
  | A.Not e1 -> A.ExprNode (A.Not (subst x v e1))
  | A.If (c, t, f) -> A.ExprNode (A.If (subst x v c, subst x v t, subst x v f))
  | A.Pair (e1, e2) -> A.ExprNode (A.Pair (subst x v e1, subst x v e2))
  | A.First e1 -> A.ExprNode (A.First (subst x v e1))
  | A.Second e1 -> A.ExprNode (A.Second (subst x v e1))
  | A.FuncApp (e1, e2) -> A.ExprNode (A.FuncApp (subst x v e1, subst x v e2))
  | A.LoopApp (e1, e2, n) -> A.ExprNode (A.LoopApp (subst x v e1, subst x v e2, n))
  | A.Observe e1 -> A.ExprNode (A.Observe (subst x v e1))
  | A.Cons (hd, tl) -> A.ExprNode (A.Cons (subst x v hd, subst x v tl))
  | A.MatchList (scrut, nil_br, xh, xt, cons_br) ->
      let scrut' = subst x v scrut in
      let nil_br' = subst x v nil_br in
      let cons_br' =
        if xh = x || xt = x then cons_br
        else
          let fv_v = free_vars v in
          let cons_br =
            if StringSet.mem xh fv_v then (
              let avoid = StringSet.union (free_vars cons_br) fv_v |> StringSet.add x in
              let xh' = fresh_name avoid xh in
              rename xh xh' cons_br)
            else cons_br
          in
          let cons_br =
            if StringSet.mem xt fv_v then (
              let avoid = StringSet.union (free_vars cons_br) fv_v |> StringSet.add x in
              let xt' = fresh_name avoid xt in
              rename xt xt' cons_br)
            else cons_br
          in
          subst x v cons_br
      in
      A.ExprNode (A.MatchList (scrut', nil_br', xh, xt, cons_br'))
  | A.Ref e1 -> A.ExprNode (A.Ref (subst x v e1))
  | A.Deref e1 -> A.ExprNode (A.Deref (subst x v e1))
  | A.Assign (e1, e2) -> A.ExprNode (A.Assign (subst x v e1, subst x v e2))
  | A.Seq (e1, e2) -> A.ExprNode (A.Seq (subst x v e1, subst x v e2))

(* ---------- Values ---------- *)

let rec is_value (A.ExprNode e : A.expr) =
  match e with
  | A.Const _ | A.BoolConst _ | A.FinConst _ | A.Fun _ | A.Fix _ | A.Unit | A.Nil -> true
  | A.Pair (a, b) -> is_value a && is_value b
  | A.Cons (a, b) -> is_value a && is_value b
  | _ -> false

(* ---------- Eager simplification ---------- *)

let rec simplify (A.ExprNode e : A.expr) : A.expr =
  let simp = simplify in
  match e with
  | A.Cmp (op, a, b, flipped) ->
      let a' = simp a in
      let b' = simp b in
      (match (a', b') with
       | A.ExprNode (A.Const x), A.ExprNode (A.Const y) ->
           let r = match op with A.Lt -> x < y | A.Le -> x <= y in
           A.ExprNode (A.BoolConst r)
       | _ -> A.ExprNode (A.Cmp (op, a', b', flipped)))
  | A.FinCmp (op, a, b, n, flipped) ->
      let a' = simp a in
      let b' = simp b in
      (match (a', b') with
       | A.ExprNode (A.FinConst (k1, n1)), A.ExprNode (A.FinConst (k2, n2))
         when n1 = n && n2 = n ->
           let r = match op with A.Lt -> k1 < k2 | A.Le -> k1 <= k2 in
           A.ExprNode (A.BoolConst r)
       | _ -> A.ExprNode (A.FinCmp (op, a', b', n, flipped)))
  | A.FinEq (a, b, n) ->
      let a' = simp a in
      let b' = simp b in
      (match (a', b') with
       | A.ExprNode (A.FinConst (k1, n1)), A.ExprNode (A.FinConst (k2, n2))
         when n1 = n && n2 = n ->
           A.ExprNode (A.BoolConst (k1 = k2))
       | _ -> A.ExprNode (A.FinEq (a', b', n)))
  | A.And (a, b) ->
      let a' = simp a in
      (match a' with
       | A.ExprNode (A.BoolConst false) -> a'
       | A.ExprNode (A.BoolConst true) -> simp b
       | _ -> A.ExprNode (A.And (a', simp b)))
  | A.Or (a, b) ->
      let a' = simp a in
      (match a' with
       | A.ExprNode (A.BoolConst true) -> a'
       | A.ExprNode (A.BoolConst false) -> simp b
       | _ -> A.ExprNode (A.Or (a', simp b)))
  | A.Not a ->
      let a' = simp a in
      (match a' with
       | A.ExprNode (A.BoolConst b) -> A.ExprNode (A.BoolConst (not b))
       | _ -> A.ExprNode (A.Not a'))
  | A.If (c, t, f) ->
      let c' = simp c in
      (match c' with
       | A.ExprNode (A.BoolConst true) -> simp t
       | A.ExprNode (A.BoolConst false) -> simp f
       | _ -> A.ExprNode (A.If (c', simp t, simp f)))
  | A.First a ->
      let a' = simp a in
      (match a' with
       | A.ExprNode (A.Pair (v1, v2)) when is_value v1 && is_value v2 -> v1
       | _ -> A.ExprNode (A.First a'))
  | A.Second a ->
      let a' = simp a in
      (match a' with
       | A.ExprNode (A.Pair (v1, v2)) when is_value v1 && is_value v2 -> v2
       | _ -> A.ExprNode (A.Second a'))
  | A.Pair (a, b) -> A.ExprNode (A.Pair (simp a, simp b))
  | A.Cons (a, b) -> A.ExprNode (A.Cons (simp a, simp b))
  | A.FuncApp (e1, e2) -> A.ExprNode (A.FuncApp (simp e1, simp e2))
  | A.LoopApp (e1, e2, n) -> A.ExprNode (A.LoopApp (simp e1, simp e2, n))
  | A.Let (x, e1, e2) -> A.ExprNode (A.Let (x, simp e1, simp e2))
  | A.DistrCase cases -> A.ExprNode (A.DistrCase (List.map (fun (ei, p) -> (simp ei, p)) cases))
  | A.Sample s ->
      (match s with
       | A.Distr1 (k, e1) -> A.ExprNode (A.Sample (A.Distr1 (k, simp e1)))
       | A.Distr2 (k, e1, e2) -> A.ExprNode (A.Sample (A.Distr2 (k, simp e1, simp e2))))
  | A.MatchList (scrut, nil_br, x, xs, cons_br) ->
      A.ExprNode (A.MatchList (simp scrut, simp nil_br, x, xs, simp cons_br))
  | A.Ref e1 -> A.ExprNode (A.Ref (simp e1))
  | A.Deref e1 -> A.ExprNode (A.Deref (simp e1))
  | A.Assign (e1, e2) -> A.ExprNode (A.Assign (simp e1, simp e2))
  | A.Seq (e1, e2) -> A.ExprNode (A.Seq (simp e1, simp e2))
  | A.Observe e1 -> A.ExprNode (A.Observe (simp e1))
  | A.Var _ | A.Const _ | A.BoolConst _ | A.FinConst _ | A.Fun _ | A.Fix _ | A.Unit | A.Nil
  | A.RuntimeError _ ->
      A.ExprNode e

(***** ---------- Small-step semantics: e -> { (p_i, successors_i) } ---------- *****)

let check_prob (p : float) : unit =
  if p < 0.0 || p > 1.0 || Float.is_nan p || Float.is_infinite p then
    failwith (Format.asprintf "invalid probability %g" p)

let check_sum_to_one (ps : float list) : unit =
  let s = List.fold_left ( +. ) 0.0 ps in
  let eps = 1e-12 in
  if Float.abs (s -. 1.0) > eps then
    failwith (Format.asprintf "probabilities must sum to 1, got %0.17g" s)

type succ =
  | Next of A.expr
  | Fail  (* observe failure *)

let map_succ (plug : A.expr -> A.expr) ((p, s) : float * succ) : float * succ =
  match s with
  | Fail -> (p, Fail)
  | Next e' -> (p, Next (plug e'))

let rec step (e : A.expr) : (float * succ) list =
  let e = simplify e in
  if is_value e then
    []
  else
    match e with
    | A.ExprNode (A.If (c, tbr, fbr)) ->
        if not (is_value c) then
          List.map (map_succ (fun c' -> A.ExprNode (A.If (c', tbr, fbr)))) (step c)
        else
          (match c with
           | A.ExprNode (A.BoolConst true) -> [ (1.0, Next tbr) ]
           | A.ExprNode (A.BoolConst false) -> [ (1.0, Next fbr) ]
           | _ -> failwith "if: condition is not a bool")
    | A.ExprNode (A.Let (x, e1, e2)) ->
        if not (is_value e1) then
          List.map (map_succ (fun e1' -> A.ExprNode (A.Let (x, e1', e2)))) (step e1)
        else
          let e' = subst x e1 e2 |> simplify in
          [ (1.0, Next e') ]
    | A.ExprNode (A.FuncApp (e1, e2)) ->
        if not (is_value e1) then
          List.map (map_succ (fun e1' -> A.ExprNode (A.FuncApp (e1', e2)))) (step e1)
        else if not (is_value e2) then
          List.map (map_succ (fun e2' -> A.ExprNode (A.FuncApp (e1, e2')))) (step e2)
        else
          (match e1 with
           | A.ExprNode (A.Fun (x, body)) ->
               let e' = subst x e2 body |> simplify in
               [ (1.0, Next e') ]
           | A.ExprNode (A.Fix (f, x, body)) ->
               let self = e1 in
               let e' = subst f self body |> subst x e2 |> simplify in
               [ (1.0, Next e') ]
           | _ -> failwith "application to non-function")
    | A.ExprNode (A.LoopApp (e1, e2, n)) ->
        if not (is_value e1) then
          List.map (map_succ (fun e1' -> A.ExprNode (A.LoopApp (e1', e2, n)))) (step e1)
        else if not (is_value e2) then
          List.map (map_succ (fun e2' -> A.ExprNode (A.LoopApp (e1, e2', n)))) (step e2)
        else
          (match e1 with
           | A.ExprNode (A.Fun (x, body)) ->
               let e' = subst x e2 body |> simplify in
               [ (1.0, Next e') ]
           | A.ExprNode (A.Fix (f, x, body)) ->
               let self = e1 in
               let e' = subst f self body |> subst x e2 |> simplify in
               [ (1.0, Next e') ]
           | _ -> failwith "loop application to non-function")
    | A.ExprNode (A.Cmp (op, a, b, flipped)) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.Cmp (op, a', b, flipped)))) (step a)
        else if not (is_value b) then
          List.map (map_succ (fun b' -> A.ExprNode (A.Cmp (op, a, b', flipped)))) (step b)
        else
          (match (a, b) with
           | A.ExprNode (A.Const x), A.ExprNode (A.Const y) ->
               let r = match op with A.Lt -> x < y | A.Le -> x <= y in
               [ (1.0, Next (A.ExprNode (A.BoolConst r))) ]
           | _ -> failwith "cmp: expected floats")
    | A.ExprNode (A.FinCmp (op, a, b, n, flipped)) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.FinCmp (op, a', b, n, flipped)))) (step a)
        else if not (is_value b) then
          List.map (map_succ (fun b' -> A.ExprNode (A.FinCmp (op, a, b', n, flipped)))) (step b)
        else
          (match (a, b) with
           | A.ExprNode (A.FinConst (k1, n1)), A.ExprNode (A.FinConst (k2, n2))
             when n1 = n && n2 = n ->
               let r = match op with A.Lt -> k1 < k2 | A.Le -> k1 <= k2 in
               [ (1.0, Next (A.ExprNode (A.BoolConst r))) ]
           | _ -> failwith "fincmp: expected Fin")
    | A.ExprNode (A.FinEq (a, b, n)) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.FinEq (a', b, n)))) (step a)
        else if not (is_value b) then
          List.map (map_succ (fun b' -> A.ExprNode (A.FinEq (a, b', n)))) (step b)
        else
          (match (a, b) with
           | A.ExprNode (A.FinConst (k1, n1)), A.ExprNode (A.FinConst (k2, n2))
             when n1 = n && n2 = n ->
               [ (1.0, Next (A.ExprNode (A.BoolConst (k1 = k2)))) ]
           | _ -> failwith "fineq: expected Fin")
    | A.ExprNode (A.And (a, b)) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.And (a', b)))) (step a)
        else
          (match a with
           | A.ExprNode (A.BoolConst false) -> [ (1.0, Next a) ]
           | A.ExprNode (A.BoolConst true) -> [ (1.0, Next b) ]
           | _ -> failwith "and: expected bool")
    | A.ExprNode (A.Or (a, b)) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.Or (a', b)))) (step a)
        else
          (match a with
           | A.ExprNode (A.BoolConst true) -> [ (1.0, Next a) ]
           | A.ExprNode (A.BoolConst false) -> [ (1.0, Next b) ]
           | _ -> failwith "or: expected bool")
    | A.ExprNode (A.Not a) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.Not a'))) (step a)
        else
          (match a with
           | A.ExprNode (A.BoolConst b) -> [ (1.0, Next (A.ExprNode (A.BoolConst (not b)))) ]
           | _ -> failwith "not: expected bool")
    | A.ExprNode (A.Pair (a, b)) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.Pair (a', b)))) (step a)
        else if not (is_value b) then
          List.map (map_succ (fun b' -> A.ExprNode (A.Pair (a, b')))) (step b)
        else
          []
    | A.ExprNode (A.First a) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.First a'))) (step a)
        else
          (match a with
           | A.ExprNode (A.Pair (v1, v2)) when is_value v1 && is_value v2 ->
               [ (1.0, Next v1) ]
           | _ -> failwith "first: expected pair")
    | A.ExprNode (A.Second a) ->
        if not (is_value a) then
          List.map (map_succ (fun a' -> A.ExprNode (A.Second a'))) (step a)
        else
          (match a with
           | A.ExprNode (A.Pair (v1, v2)) when is_value v1 && is_value v2 ->
               [ (1.0, Next v2) ]
           | _ -> failwith "second: expected pair")
    | A.ExprNode (A.Cons (hd, tl)) ->
        if not (is_value hd) then
          List.map (map_succ (fun hd' -> A.ExprNode (A.Cons (hd', tl)))) (step hd)
        else if not (is_value tl) then
          List.map (map_succ (fun tl' -> A.ExprNode (A.Cons (hd, tl')))) (step tl)
        else
          []
    | A.ExprNode (A.MatchList (scrut, nil_br, x, xs, cons_br)) ->
        if not (is_value scrut) then
          List.map
            (map_succ (fun s' -> A.ExprNode (A.MatchList (s', nil_br, x, xs, cons_br))))
            (step scrut)
        else
          (match scrut with
           | A.ExprNode A.Nil -> [ (1.0, Next nil_br) ]
           | A.ExprNode (A.Cons (vhd, vtl)) when is_value vhd && is_value vtl ->
               let e' = subst xs vtl (subst x vhd cons_br) |> simplify in
               [ (1.0, Next e') ]
           | _ -> failwith "matchlist: expected [] or v::vs")
    | A.ExprNode (A.DistrCase cases) ->
        if cases = [] then failwith "distrcase: empty"
        else
          let ps = List.map snd cases in
          List.iter check_prob ps;
          check_sum_to_one ps;
          List.map (fun (ei, p) -> (p, Next ei)) cases
    | A.ExprNode (A.Sample _) ->
        failwith "to_mc: continuous distributions not supported"
    | A.ExprNode (A.Observe c) ->
        if not (is_value c) then
          List.map (map_succ (fun c' -> A.ExprNode (A.Observe c'))) (step c)
        else
          (match c with
           | A.ExprNode (A.BoolConst true) -> [ (1.0, Next (A.ExprNode A.Unit)) ]
           | A.ExprNode (A.BoolConst false) -> [ (1.0, Fail) ]
           | _ -> failwith "observe: expected bool")
    | A.ExprNode (A.Seq (e1, e2)) ->
        if not (is_value e1) then
          List.map (map_succ (fun e1' -> A.ExprNode (A.Seq (e1', e2)))) (step e1)
        else
          [ (1.0, Next e2) ]
    | A.ExprNode (A.Ref _ | A.Deref _ | A.Assign _) ->
        failwith "to_mc: refs not supported (yet)"
    | A.ExprNode (A.Var _) ->
        failwith "to_mc: stuck on Var (did you forget to substitute?)"
    | A.ExprNode (A.Unit | A.Nil | A.BoolConst _ | A.Const _ | A.FinConst _ | A.Fun _ | A.Fix _ | A.RuntimeError _) ->
        []

(* ---------- Graph construction ---------- *)

let prob_is_one (p : float) : bool =
  Float.abs (p -. 1.0) <= 1e-12

(* Collapse short deterministic chains (p=1, single successor). *)
let normalize_state (e0 : A.expr) : A.expr =
  let max_collapse_steps = 64 in
  let seen : (A.expr, unit) Hashtbl.t = Hashtbl.create 64 in
  let rec loop fuel (e : A.expr) : A.expr =
    let e = simplify e in
    if fuel <= 0 || Hashtbl.mem seen e then
      e
    else begin
      Hashtbl.add seen e ();
      match step e with
      | [ (p, Next e') ] when prob_is_one p -> loop (fuel - 1) e'
      | _ -> e
    end
  in
  loop max_collapse_steps e0

let expr_key (e : A.expr) : string =
  let h = Hashtbl.hash e in
  string_of_int h ^ ":" ^ Marshal.to_string e [ Marshal.No_sharing ]

let merge_transitions (ts : transition list) : transition list =
  let tbl : ((int * int), float) Hashtbl.t = Hashtbl.create (max 16 (List.length ts)) in
  List.iter
    (fun { src; dst; prob } ->
      let key = (src, dst) in
      let prev =
        match Hashtbl.find_opt tbl key with
        | Some p -> p
        | None -> 0.0
      in
      Hashtbl.replace tbl key (prev +. prob))
    ts;
  Hashtbl.fold
    (fun (src, dst) prob acc ->
      if prob <= 0.0 then acc else { src; dst; prob } :: acc)
    tbl
    []

let to_mc (e0 : A.expr) : mc =
  let e0 = normalize_state e0 in

  let id_of : (string, int) Hashtbl.t = Hashtbl.create 4096 in
  let expr_of : (int, A.expr) Hashtbl.t = Hashtbl.create 4096 in
  let q : int Queue.t = Queue.create () in
  let next_id = ref 0 in

  let add_state (e : A.expr) : int option =
    let e = normalize_state e in
    let key = expr_key e in
    match Hashtbl.find_opt id_of key with
    | Some id -> Some id
    | None ->
        let id = !next_id in
        incr next_id;
        Hashtbl.add id_of key id;
        Hashtbl.add expr_of id e;
        Queue.add id q;
        Some id
  in

  let start_id =
    match add_state e0 with
    | Some id -> id
    | None -> failwith "to_mc: unable to add initial state"
  in
  if start_id <> 0 then failwith "internal error: initial state is not 0";

  let transitions_rev : transition list ref = ref [] in
  let terminal_ids : int list ref = ref [] in
  let fail_srcs : (int * float) list ref = ref [] in
  let error_srcs : int list ref = ref [] in
  let has_fail = ref false in (* if a program has_fail, then it will has_observe *)

  while not (Queue.is_empty q) do
    let sid = Queue.take q in
    let e = Hashtbl.find expr_of sid in
    if is_value (simplify e) then
      terminal_ids := sid :: !terminal_ids
    else begin
      let succs = step e in
      if succs = [] then begin
        if contains_runtime_error e then error_srcs := sid :: !error_srcs
        else
          failwith
            "to_mc: stuck state is neither a returned value nor a runtime error"
      end
      else
        List.iter
          (fun (p, s) ->
            match s with
            | Fail ->
                has_fail := true;
                fail_srcs := (sid, p) :: !fail_srcs
            | Next e' ->
                match add_state e' with
                | Some tid ->
                    transitions_rev := { src = sid; dst = tid; prob = p } :: !transitions_rev
                | None -> ())
          succs
    end
  done;

  (* Allocate successful-return, observation-failure, and runtime-error sinks. *)
  let done_id = !next_id in
  let fail_id =
    if !has_fail then Some (done_id + 1) else None
  in
  let error_id =
    if !error_srcs = [] then None
    else Some (done_id + 1 + if !has_fail then 1 else 0)
  in
  let num_states =
    match error_id, fail_id with
    | Some eid, _ -> eid + 1
    | None, Some fid -> fid + 1
    | None, None -> done_id + 1
  in
  let base = !transitions_rev in
  let from_terminals =
    List.map (fun sid -> { src = sid; dst = done_id; prob = 1.0 }) !terminal_ids
  in
  let done_self = { src = done_id; dst = done_id; prob = 1.0 } in
  let fail_edges, fail_self =
    match fail_id with
    | None -> ([], [])
    | Some fid ->
        ( List.map (fun (sid, p) -> { src = sid; dst = fid; prob = p }) !fail_srcs
        , [ { src = fid; dst = fid; prob = 1.0 } ] )
  in
  let error_edges, error_self =
    match error_id with
    | None -> ([], [])
    | Some eid ->
        ( List.map (fun sid -> { src = sid; dst = eid; prob = 1.0 }) !error_srcs,
          [ { src = eid; dst = eid; prob = 1.0 } ] )
  in
  (* Sort transitions for nicer output *)
  let merged =
    merge_transitions
      (done_self
       :: (error_self @ error_edges @ fail_self @ fail_edges @ from_terminals @ base))
  in
  let transitions =
    List.sort
      (fun a b ->
        match compare a.src b.src with
        | 0 -> (match compare a.dst b.dst with 0 -> compare a.prob b.prob | c -> c)
        | c -> c)
      merged
  in
  { num_states; sink = done_id; transitions; expr_of; has_observe = !has_fail;
    fail = fail_id; error = error_id }

(* Transition file (.tra) for STORM backend *)
let write_tra_file (m : mc) (path : string) : unit =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "dtmc@.";
  List.iter
    (fun { src; dst; prob } ->
      Format.fprintf fmt "%d %d %.17g@." src dst prob)
    m.transitions;
  close_out oc

(* Labeling file (.lab) for STORM backend *)
let write_lab_file (m : mc) (path : string) : unit =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "#DECLARATION@.";
  Format.fprintf fmt "init done accept";
  if m.has_observe then Format.fprintf fmt " fail has_observe";
  if Option.is_some m.error then Format.fprintf fmt " error";
  Format.fprintf fmt "@.";
  Format.fprintf fmt "#END@.";
  Format.fprintf fmt "0 init@.";
  for sid = 0 to (m.num_states - 1) do
    match Hashtbl.find_opt m.expr_of sid with
    | Some (A.ExprNode (A.BoolConst true)) ->
        Format.fprintf fmt "%d accept@." sid (* label only true states with accept *)
    | _ -> ()
  done;
  Format.fprintf fmt "%d done@." m.sink;
  (match m.fail with
   | None -> ()
   | Some fid -> Format.fprintf fmt "%d fail@." fid);
  (match m.error with
   | None -> ()
   | Some eid -> Format.fprintf fmt "%d error@." eid);
  close_out oc
