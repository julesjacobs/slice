(**
 * @file ast.ml
 * @brief Defines the core Abstract Syntax Tree (AST) structures for the ContDice language.
 *
 * This module contains the type definitions for expressions, different kinds of
 * distributions, and the generic structure of AST nodes. These types form the
 * fundamental representation of ContDice programs before type checking and
 * evaluation.
 *)
(**
 * @file ast.mli
 * @brief Interface for the core Abstract Syntax Tree (AST) structures of the ContDice language.
 *
 * This module defines the types for representing expressions in ContDice. It includes
 * a generic expression structure (`expr_generic`), specific kinds for single and
 * two-argument distributions, and the concrete `expr` type used for program representation.
 *)

(**
 * @type 'a expr_generic
 * @brief Generic structure for expressions, parameterized by the type of sub-expressions `'a`.
 *
 * This type defines all possible nodes in the AST.
 *)
type 'a expr_generic =
  | Var of string  (** Variable identifier, e.g., `x` *)
  | Const of float  (** Floating-point constant, e.g., `3.14` *)
  | BoolConst of bool  (** Boolean constant, e.g., `true` or `false` *)
  | Let of string * 'a * 'a  (** Let binding: `let x = e1 in e2` *)
  | Sample of 'a sample  (** Sampling from a probability distribution *)
  | DistrCase of ('a * float) list  (** Discrete distribution defined by cases: `discrete(p1:e1, p2:e2, ...)` *)
  | Less of 'a * 'a  (** Less than comparison: `e1 < e2` *)
  | LessEq of 'a * 'a  (** Less than or equal to comparison: `e1 <= e2` *)
  | And of 'a * 'a  (** Logical AND: `e1 && e2` *)
  | Or of 'a * 'a  (** Logical OR: `e1 || e2` *)
  | Not of 'a  (** Logical NOT: `not e` *)
  | If of 'a * 'a * 'a  (** If-then-else expression: `if cond then e_true else e_false` *)
  | Pair of 'a * 'a  (** Pair construction: `(e1, e2)` *)
  | First of 'a  (** First projection from a pair: `fst e` *)
  | Second of 'a  (** Second projection from a pair: `snd e` *)
  | Fun of string * 'a  (** Function definition: `fun x -> e` *)
  | FuncApp of 'a * 'a  (** Function application: `e1 e2` *)
  | LoopApp of 'a * 'a * int  (** Iterated function application: `iterate(f, x, n)` *)
  | FinConst of int * int  (** Finite set constant: `k#n` (value `k` in `Z_n`) *)
  | FinLt of 'a * 'a * int  (** Finite set less than: `e1 <#n e2` *)
  | FinLeq of 'a * 'a * int  (** Finite set less than or equal: `e1 <=#n e2` *)
  | FinEq of 'a * 'a * int  (** Finite set equality: `e1 ==#n e2` *)
  | Observe of 'a  (** Observation statement: `observe e` where `e` is boolean *)
  | Fix of string * string * 'a  (** Fixed-point combinator for recursion: `fix f x := e` *)
  | Nil  (** Empty list literal: `nil` *)
  | Cons of 'a * 'a  (** List constructor: `e_head :: e_tail` *)
  | MatchList of 'a * 'a * string * string * 'a  (** List pattern matching: `match e with nil -> e_nil | y::ys -> e_cons end` *)
  | Ref of 'a  (** Reference creation: `ref e` *)
  | Deref of 'a  (** Reference dereferencing: `!e` *)
  | Assign of 'a * 'a  (** Reference assignment: `e_ref := e_val` *)
  | Seq of 'a * 'a  (** Sequencing: `e1 ; e2` *)
  | Unit  (** Unit value: `()` *)
  | RuntimeError of string  (** AST node representing a runtime error, used if an error needs to be embedded in the AST. *)

(**
 * @type single_arg_dist_kind
 * @brief Kinds of continuous probability distributions that take a single argument.
 *)
and single_arg_dist_kind =
  | DExponential  (** Exponential distribution (rate) *)
  | DLaplace  (** Laplace distribution (scale) *)
  | DCauchy  (** Cauchy distribution (scale) *)
  | DTDist  (** Student's t-distribution (degrees of freedom, nu) *)
  | DChi2  (** Chi-squared distribution (degrees of freedom, nu) *)
  | DLogistic  (** Logistic distribution (scale) *)
  | DRayleigh  (** Rayleigh distribution (scale, sigma) *)

(**
 * @type two_arg_dist_kind
 * @brief Kinds of continuous probability distributions that take two arguments.
 *)
and two_arg_dist_kind =
  | DUniform  (** Uniform distribution (low, high) *)
  | DGaussian  (** Gaussian (Normal) distribution (mean, std_dev) *)
  | DBeta  (** Beta distribution (alpha, beta) *)
  | DLogNormal  (** LogNormal distribution (mu, sigma) *)
  | DGamma  (** Gamma distribution (shape, scale) *)
  | DPareto  (** Pareto distribution (shape, scale) *)
  | DWeibull  (** Weibull distribution (shape, scale) *)
  | DGumbel1  (** Gumbel Type 1 distribution (location, scale) *)
  | DGumbel2  (** Gumbel Type 2 distribution (location, scale) *)
  | DExppow  (** Exponential Power distribution (shape, scale) *)

(**
 * @type 'a sample
 * @brief Represents a sample from a probability distribution.
 * It can be a single-argument or two-argument distribution.
 *)
and 'a sample =
  | Distr1 of single_arg_dist_kind * 'a  (** A single-argument distribution with its argument. *)
  | Distr2 of two_arg_dist_kind * 'a * 'a  (** A two-argument distribution with its arguments. *)

(**
 * @type expr
 * @brief The concrete type for expressions in the source language.
 *
 * An `expr` is an `expr_generic` where sub-expressions are also of type `expr`.
 * This forms the recursive structure of the AST. It is wrapped in `ExprNode`
 * to allow for potential future metadata at each node, though not currently used.
 *)
type expr = ExprNode of expr expr_generic
