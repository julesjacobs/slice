(* AST (Abstract Syntax Tree) types *)

type 'a expr_generic =
  | Var of string
  | Const of float
  | BoolConst of bool
  | Let of string * 'a * 'a
  | Sample of 'a sample (* Continuous distribution *)
  | DistrCase of
      ('a * float) list (* General discrete distribution: (expr * prob) list *)
  | Less of 'a * 'a
  | LessEq of 'a * 'a
  | And of 'a * 'a
  | Or of 'a * 'a
  | Not of 'a
  | If of 'a * 'a * 'a
  | Pair of 'a * 'a (* Pair construction (e1, e2) *)
  | First of 'a (* First projection: fst e *)
  | Second of 'a (* Second projection: snd e *)
  | Fun of string * 'a (* Function: fun x -> e *)
  | FuncApp of 'a * 'a (* Function application: e1 e2 *)
  | LoopApp of 'a * 'a * int (* Loop application: e1 e2 int *)
  | FinConst of int * int (* k, n for k#n *)
  | FinLt of 'a * 'a * int (* e1 <#n e2 *)
  | FinLeq of 'a * 'a * int (* e1 <=#n e2 *)
  | FinEq of 'a * 'a * int (* e1 ==#n e2 *)
  | Observe of 'a
  | Fix of string * string * 'a
  | Nil (* nil *)
  | Cons of 'a * 'a (* e1 :: e2 *)
  | MatchList of 'a * 'a * string * string * 'a
    (* match e1 with nil -> e_nil | y::ys -> e_cons end *)
  | Ref of 'a (* ref e *)
  | Deref of 'a (* !e *)
  | Assign of 'a * 'a (* e1 := e2 *)
  | Seq of 'a * 'a (* e1 ; e2 *)
  | Unit
  | RuntimeError of string (* Variant for representing a runtime error in the AST, if needed *)

and single_arg_dist_kind =
  | DExponential
  | DLaplace
  | DCauchy
  | DTDist
  | DChi2
  | DLogistic
  | DRayleigh

and two_arg_dist_kind =
  | DUniform
  | DGaussian
  | DBeta
  | DLogNormal
  | DGamma
  | DPareto
  | DWeibull
  | DGumbel1
  | DGumbel2
  | DExppow

and 'a sample =
  | Distr1 of single_arg_dist_kind * 'a
  | Distr2 of two_arg_dist_kind * 'a * 'a

(** The source language expression type *)
type expr = ExprNode of expr expr_generic
