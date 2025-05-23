open Types (* To get access to Ast, RuntimeValues etc. modules *)
open Ast (* To get access to single_arg_dist_kind, two_arg_dist_kind, and their constructors directly *)

(**
 * @file distributions.ml
 * @brief Defines continuous probability distributions and related operations for ContDice.
 *
 * This module provides:
 * - The `cdistr` type to represent various continuous probability distributions
 *   (e.g., Uniform, Gaussian, Exponential).
 * - Functions for sampling from these distributions (`cdistr_sample`).
 * - Functions for calculating the Cumulative Distribution Function (CDF) (`cdistr_cdf`).
 * - Helper functions to convert AST distribution kinds (`Ast.single_arg_dist_kind`,
 *   `Ast.two_arg_dist_kind`) into `cdistr` values and to sample/calculate CDFs
 *   using these kinds.
 *
 * It relies on the GNU Scientific Library (GSL) via the `Gsl` OCaml bindings for
 * the underlying statistical computations.
 *)

(* ----------------------------------------------------------------------- *)
(*  Type definition for Continuous Distributions                            *)
(* ----------------------------------------------------------------------- *)

(**
 * @type cdistr
 * @brief Represents a specific continuous probability distribution with its parameters.
 * This type is used at runtime by the interpreter to perform sampling and other
 * distribution-related operations.
 *)
type cdistr =
  | Uniform of float * float  (** Uniform distribution defined by `low` and `high` bounds. *)
  | Gaussian of float * float  (** Gaussian (Normal) distribution defined by `mean` and `standard deviation`. *)
  | Exponential of float  (** Exponential distribution defined by `rate` (lambda). Note: GSL uses `mu = 1/lambda`. *)
  | Beta of float * float  (** Beta distribution defined by shape parameters `alpha` and `beta`. *)
  | LogNormal of float * float  (** LogNormal distribution defined by `mu` (mean of log) and `sigma` (std dev of log). *)
  | Gamma of float * float  (** Gamma distribution defined by `shape` and `scale` parameters. *)
  | Laplace of float  (** Laplace distribution defined by `scale` parameter (GSL's `a`). *)
  | Cauchy of float  (** Cauchy distribution defined by `scale` parameter (GSL's `a`). *)
  | Pareto of float * float  (** Pareto distribution defined by `shape` (a) and `scale` (b, minimum value). *)
  | Weibull of float * float  (** Weibull distribution defined by `shape` (a) and `scale` (b). *)
  | TDist of float  (** Student's t-distribution defined by degrees of freedom `nu`. *)
  | Chi2 of float  (** Chi-squared distribution defined by degrees of freedom `nu`. *)
  | Logistic of float  (** Logistic distribution defined by `scale` parameter (GSL's `a`). *)
  | Gumbel1 of float * float  (** Gumbel Type 1 (max) distribution defined by `location` (a) and `scale` (b). *)
  | Gumbel2 of float * float  (** Gumbel Type 2 (min) distribution defined by `location` (a) and `scale` (b). *)
  | Rayleigh of float  (** Rayleigh distribution defined by `scale` parameter `sigma`. *)
  | Exppow of float * float  (** Exponential Power distribution defined by `shape` (a) and `scale` (b). *)

(** Global Random Number Generator (RNG) instance from GSL.
    Initialized once when the module is loaded. Uses the default GSL RNG type. *)
let rng = Gsl.Rng.make (Gsl.Rng.default ())

(**
 * @function cdistr_cdf
 * @brief Calculates the Cumulative Distribution Function (CDF) for a given distribution at point `x`.
 * P(X <= x).
 *
 * Validates distribution parameters before calling the corresponding GSL CDF function.
 * Handles edge cases for `x = neg_infinity` (CDF is 0.0) and `x = infinity` (CDF is 1.0).
 *
 * @param dist The `cdistr` instance.
 * @param x The point at which to evaluate the CDF.
 * @return The CDF value (float between 0.0 and 1.0).
 * @raise Invalid_argument if distribution parameters are invalid (e.g., non-positive scale).
 *)
let cdistr_cdf dist x =
  if x = neg_infinity then 0.0
  else if x = infinity then 1.0
  else
    match dist with
    | Uniform (lo, hi) ->
        if lo > hi then invalid_arg "Uniform CDF: lo must be ≤ hi"
        else Gsl.Cdf.flat_P ~x ~a:lo ~b:hi
    | Gaussian (m, s) ->
        if s <= 0.0 then invalid_arg "Gaussian CDF: σ must be > 0";
        Gsl.Cdf.gaussian_P ~x:(x -. m) ~sigma:s
    | Exponential lambda ->
        if lambda <= 0.0 then invalid_arg "Exponential CDF: λ must be > 0";
        Gsl.Cdf.exponential_P ~x ~mu:(1.0 /. lambda)
    | Beta (alpha, beta_param) ->
        if alpha <= 0.0 || beta_param <= 0.0 then
          invalid_arg "Beta CDF: α and β must be > 0"
        else Gsl.Cdf.beta_P ~x ~a:alpha ~b:beta_param
    | LogNormal (mu, sigma) ->
        if sigma <= 0.0 then invalid_arg "LogNormal CDF: σ must be > 0";
        Gsl.Cdf.lognormal_P ~x ~zeta:mu ~sigma
    | Gamma (a, b) ->
        if a <= 0.0 || b <= 0.0 then
          invalid_arg "Gamma CDF: shape and scale must be > 0";
        Gsl.Cdf.gamma_P ~x ~a ~b
    | Laplace a ->
        if a <= 0.0 then invalid_arg "Laplace CDF: scale must be > 0";
        Gsl.Cdf.laplace_P ~x ~a
    | Cauchy a ->
        if a <= 0.0 then invalid_arg "Cauchy CDF: scale must be > 0";
        Gsl.Cdf.cauchy_P ~x ~a
    | Pareto (a, b) ->
        if a <= 0.0 || b <= 0.0 then
          invalid_arg "Pareto CDF: a and b must be > 0";
        Gsl.Cdf.pareto_P ~x ~a ~b
    | Weibull (a, b) ->
        if a <= 0.0 || b <= 0.0 then
          invalid_arg "Weibull CDF: a and b must be > 0";
        Gsl.Cdf.weibull_P ~x ~a ~b
    | TDist nu ->
        if nu <= 0.0 then invalid_arg "TDist CDF: nu must be > 0";
        Gsl.Cdf.tdist_P ~x ~nu
    | Chi2 nu ->
        if nu <= 0.0 then invalid_arg "Chi2 CDF: nu must be > 0";
        Gsl.Cdf.chisq_P ~x ~nu
    | Logistic a ->
        if a <= 0.0 then invalid_arg "Logistic CDF: scale must be > 0";
        Gsl.Cdf.logistic_P ~x ~a
    | Gumbel1 (a, b) ->
        if b <= 0.0 then invalid_arg "Gumbel1 CDF: b must be > 0";
        Gsl.Cdf.gumbel1_P ~x ~a ~b
    | Gumbel2 (a, b) ->
        if b <= 0.0 then invalid_arg "Gumbel2 CDF: b must be > 0";
        Gsl.Cdf.gumbel2_P ~x ~a ~b
    | Rayleigh sigma ->
        if sigma <= 0.0 then invalid_arg "Rayleigh CDF: sigma must be > 0";
        Gsl.Cdf.rayleigh_P ~x ~sigma
    | Exppow (a, b) ->
        if a <= 0.0 || b <= 0.0 then
          invalid_arg "Exppow CDF: a and b must be > 0";
        Gsl.Cdf.exppow_P ~x ~a ~b

(**
 * @function cdistr_sample
 * @brief Samples a random value from the given continuous distribution.
 *
 * Validates distribution parameters before calling the corresponding GSL random variate function.
 * Uses the global `rng` for random number generation.
 *
 * @param dist The `cdistr` instance to sample from.
 * @return A float representing the sampled value.
 * @raise Invalid_argument if distribution parameters are invalid.
 *)
let cdistr_sample dist =
  match dist with
  | Uniform (lo, hi) ->
      if lo > hi then invalid_arg "Uniform sample: lo must be ≤ hi";
      (* Handle case where lo = hi to avoid GSL error for zero-width interval *)
      if lo = hi then lo else Gsl.Randist.flat rng ~a:lo ~b:hi
  | Gaussian (mu, sigma) ->
      if sigma <= 0.0 then invalid_arg "Gaussian sample: standard deviation (σ) must be > 0";
      mu +. Gsl.Randist.gaussian rng ~sigma (* GSL's gaussian takes sigma, returns value to be added to mean *)
  | Exponential lambda ->
      if lambda <= 0.0 then invalid_arg "Exponential sample: lambda must be > 0";
      Gsl.Randist.exponential rng ~mu:(1.0 /. lambda)
  | Beta (alpha, beta_param) ->
      if alpha <= 0.0 || beta_param <= 0.0 then
        invalid_arg "Beta sample: α and β must be > 0";
      Gsl.Randist.beta rng ~a:alpha ~b:beta_param
  | LogNormal (mu, sigma) ->
      if sigma <= 0.0 then invalid_arg "LogNormal sample: σ must be > 0";
      Gsl.Randist.lognormal rng ~zeta:mu ~sigma
  | Gamma (a, b) ->
      if a <= 0.0 || b <= 0.0 then
        invalid_arg "Gamma sample: shape and scale must be > 0";
      Gsl.Randist.gamma rng ~a ~b
  | Laplace a ->
      if a <= 0.0 then invalid_arg "Laplace sample: scale must be > 0";
      Gsl.Randist.laplace rng ~a
  | Cauchy a ->
      if a <= 0.0 then invalid_arg "Cauchy sample: scale must be > 0";
      Gsl.Randist.cauchy rng ~a
  | Pareto (a, b) ->
      if a <= 0.0 || b <= 0.0 then
        invalid_arg "Pareto sample: a and b must be > 0";
      Gsl.Randist.pareto rng ~a ~b
  | Weibull (a, b) ->
      if a <= 0.0 || b <= 0.0 then
        invalid_arg "Weibull sample: a and b must be > 0";
      Gsl.Randist.weibull rng ~a ~b
  | TDist nu ->
      if nu <= 0.0 then invalid_arg "TDist sample: nu must be > 0";
      Gsl.Randist.tdist rng ~nu
  | Chi2 nu ->
      if nu <= 0.0 then invalid_arg "Chi2 sample: nu must be > 0";
      Gsl.Randist.chisq rng ~nu
  | Logistic a ->
      if a <= 0.0 then invalid_arg "Logistic sample: scale must be > 0";
      Gsl.Randist.logistic rng ~a
  | Gumbel1 (a, b) ->
      if b <= 0.0 then invalid_arg "Gumbel1 sample: b must be > 0";
      Gsl.Randist.gumbel1 rng ~a ~b
  | Gumbel2 (a, b) ->
      if b <= 0.0 then invalid_arg "Gumbel2 sample: b must be > 0";
      Gsl.Randist.gumbel2 rng ~a ~b
  | Rayleigh sigma ->
      if sigma <= 0.0 then invalid_arg "Rayleigh sample: sigma must be > 0";
      Gsl.Randist.rayleigh rng ~sigma
  | Exppow (a, b) ->
      if a <= 0.0 || b <= 0.0 then
        invalid_arg "Exppow sample: a and b must be > 0";
      Gsl.Randist.exppow rng ~a ~b

(* --- Helper Functions for Converting AST Kinds to cdistr and Operations --- *)

(**
 * @function string_of_single_arg_dist_kind
 * @brief Converts a `single_arg_dist_kind` from `Ast.ml` to its string name.
 * Used primarily for error messages and debugging.
 *
 * @param kind The `single_arg_dist_kind` variant.
 * @return The string name of the distribution kind (e.g., "exponential").
 *)
let string_of_single_arg_dist_kind (kind : single_arg_dist_kind) : string =
  match kind with
  | DExponential -> "exponential" (* Represents Ast.DExponential *)
  | DLaplace -> "laplace"
  | DCauchy -> "cauchy"
  | DTDist -> "tdist"
  | DChi2 -> "chi2"
  | DLogistic -> "logistic"
  | DRayleigh -> "rayleigh"

(**
 * @function string_of_two_arg_dist_kind
 * @brief Converts a `two_arg_dist_kind` from `Ast.ml` to its string name.
 * Used primarily for error messages and debugging.
 *
 * @param kind The `two_arg_dist_kind` variant.
 * @return The string name of the distribution kind (e.g., "uniform").
 *)
let string_of_two_arg_dist_kind (kind : two_arg_dist_kind) : string =
  match kind with
  | DUniform -> "uniform" (* Represents Ast.DUniform *)
  | DGaussian -> "gaussian"
  | DBeta -> "beta"
  | DLogNormal -> "lognormal"
  | DGamma -> "gamma"
  | DPareto -> "pareto"
  | DWeibull -> "weibull"
  | DGumbel1 -> "gumbel1"
  | DGumbel2 -> "gumbel2"
  | DExppow -> "exppow"

(**
 * @function get_cdistr_from_single_arg_kind
 * @brief Creates a `cdistr` value from an `Ast.single_arg_dist_kind` and its evaluated argument.
 * This function does not perform parameter validation itself; validation is done by
 * `cdistr_sample` or `cdistr_cdf` when the `cdistr` is used.
 *
 * @param kind The `single_arg_dist_kind` (e.g., `Ast.DExponential`).
 * @param arg1 The float argument for the distribution.
 * @return The corresponding `cdistr` value (e.g., `Exponential arg1`).
 *)
let get_cdistr_from_single_arg_kind (kind : single_arg_dist_kind)
    (arg1 : float) : cdistr =
  match kind with
  | DExponential -> Exponential arg1 (* Ast.DExponential maps to cdistr Exponential *)
  | DLaplace -> Laplace arg1
  | DCauchy -> Cauchy arg1
  | DTDist -> TDist arg1
  | DChi2 -> Chi2 arg1
  | DLogistic -> Logistic arg1
  | DRayleigh -> Rayleigh arg1

(**
 * @function get_cdistr_from_two_arg_kind
 * @brief Creates a `cdistr` value from an `Ast.two_arg_dist_kind` and its evaluated arguments.
 * This function does not perform parameter validation itself; validation is done by
 * `cdistr_sample` or `cdistr_cdf` when the `cdistr` is used.
 *
 * @param kind The `two_arg_dist_kind` (e.g., `Ast.DUniform`).
 * @param arg1 The first float argument for the distribution.
 * @param arg2 The second float argument for the distribution.
 * @return The corresponding `cdistr` value (e.g., `Uniform (arg1, arg2)`).
 *)
let get_cdistr_from_two_arg_kind (kind : two_arg_dist_kind) (arg1 : float)
    (arg2 : float) : cdistr =
  match kind with
  | DUniform -> Uniform (arg1, arg2) (* Ast.DUniform maps to cdistr Uniform *)
  | DGaussian -> Gaussian (arg1, arg2)
  | DBeta -> Beta (arg1, arg2)
  | DLogNormal -> LogNormal (arg1, arg2)
  | DGamma -> Gamma (arg1, arg2)
  | DPareto -> Pareto (arg1, arg2)
  | DWeibull -> Weibull (arg1, arg2)
  | DGumbel1 -> Gumbel1 (arg1, arg2)
  | DGumbel2 -> Gumbel2 (arg1, arg2)
  | DExppow -> Exppow (arg1, arg2)

(** Convenience function to sample directly using an AST kind and one argument. *)
let cdistr_sample_single_arg (kind : single_arg_dist_kind) (arg1 : float)
    : float =
  cdistr_sample (get_cdistr_from_single_arg_kind kind arg1)

(** Convenience function to sample directly using an AST kind and two arguments. *)
let cdistr_sample_two_arg (kind : two_arg_dist_kind) (arg1 : float)
    (arg2 : float) : float =
  cdistr_sample (get_cdistr_from_two_arg_kind kind arg1 arg2)

(** Convenience function to calculate CDF directly using an AST kind and one argument. *)
let cdistr_cdf_single_arg (kind : single_arg_dist_kind) (arg1 : float)
    (x : float) : float =
  cdistr_cdf (get_cdistr_from_single_arg_kind kind arg1) x

(** Convenience function to calculate CDF directly using an AST kind and two arguments. *)
let cdistr_cdf_two_arg (kind : two_arg_dist_kind) (arg1 : float)
    (arg2 : float) (x : float) : float =
  cdistr_cdf (get_cdistr_from_two_arg_kind kind arg1 arg2) x
