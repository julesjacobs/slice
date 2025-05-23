(*
This module defines the types for continuous probability distributions supported
in the system. It provides functions to calculate their Cumulative Distribution
Function (CDF) and to draw samples from them. These operations primarily rely on
the GNU Scientific Library (GSL). Additionally, the module includes helper
functions for creating instances of these distributions from raw float parameters,
incorporating necessary validation checks for these parameters.
*)

(* ----------------------------------------------------------------------- *)
(*  Type definition                                                         *)
(* ----------------------------------------------------------------------- *)

(** The algebraic data type representing continuous distributions.
    Each constructor holds the parameters defining that specific distribution.
    For example, `Gaussian (mean, std_dev)`. *)
type cdistr =
  | Uniform   of float * float            (** Uniform(low, high)                    *)
  | Gaussian  of float * float            (** Gaussian(mean, std_dev)               *)
  | Exponential of float                  (** Exponential(rate_lambda)              *)
  | Beta     of float * float             (** Beta(alpha, beta)                     *)
  | LogNormal of float * float            (** LogNormal(mu, sigma) – parameters of the underlying Normal dist *)
  | Gamma    of float * float             (** Gamma(shape, scale)                   *)
  | Laplace  of float                     (** Laplace(scale_b) - mean is 0          *)
  | Cauchy   of float                     (** Cauchy(scale_gamma) - median is 0     *)
  | Pareto   of float * float             (** Pareto(xm, alpha) - xm is scale, alpha is shape *)
  | Weibull  of float * float             (** Weibull(k, lambda) - k is shape, lambda is scale *)
  | TDist    of float                     (** Student's t(nu) - nu is degrees of freedom *)
  | Chi2     of float                     (** Chi-squared(nu) - nu is degrees of freedom *)
  | Logistic of float                     (** Logistic(s) - s is scale, location is 0 *)
  | Gumbel1  of float * float             (** Gumbel Type 1(mu, beta) - mu is location, beta is scale *)
  | Gumbel2  of float * float             (** Gumbel Type 2(alpha, beta) - alpha is shape, beta is scale *)
  | Rayleigh of float                     (** Rayleigh(sigma) - sigma is scale      *)
  | Exppow   of float * float             (** Exponential Power(kappa, mu) - kappa is shape, mu is scale *)

(** Global instance of GSL's random number generator.
    Initialized with GSL's default RNG type. Used by `cdistr_sample`. *)
let rng = Gsl.Rng.make (Gsl.Rng.default ())

(** [cdistr_cdf dist x] computes the Cumulative Distribution Function (CDF) 
    for the given distribution [dist] at point [x], i.e., P(X <= x).
    It handles edge cases for [x = neg_infinity] (returns 0.0) and 
    [x = infinity] (returns 1.0). Otherwise, it dispatches to the 
    corresponding GSL CDF function based on the type of [dist].
    Note: For Exponential, GSL's `exponential_P` uses `mu` (mean), 
    which is `1.0 / lambda` (rate).
*)
let cdistr_cdf dist x =
  if x = neg_infinity then 0.0
  else if x = infinity then 1.0
  else
    match dist with
  | Uniform (lo, hi)        -> Gsl.Cdf.flat_P ~x:x ~a:lo ~b:hi
  | Gaussian (m, s)         -> Gsl.Cdf.gaussian_P ~x:(x -. m) ~sigma:s (* GSL's gaussian_P is for N(0,sigma) so shift x *)
  | Exponential lambda      -> Gsl.Cdf.exponential_P ~x:x ~mu:(1.0 /. lambda)
  | Beta (alpha, beta_param) -> Gsl.Cdf.beta_P ~x:x ~a:alpha ~b:beta_param
  | LogNormal (mu, sigma)   -> Gsl.Cdf.lognormal_P ~x:x ~zeta:mu ~sigma
  | Gamma (a, b)            -> Gsl.Cdf.gamma_P ~x:x ~a ~b
  | Laplace a               -> Gsl.Cdf.laplace_P ~x:x ~a
  | Cauchy a                -> Gsl.Cdf.cauchy_P ~x:x ~a
  | Pareto (a, b)           -> Gsl.Cdf.pareto_P ~x:x ~a ~b
  | Weibull (a, b)          -> Gsl.Cdf.weibull_P ~x:x ~a ~b
  | TDist nu                -> Gsl.Cdf.tdist_P ~x:x ~nu
  | Chi2 nu                 -> Gsl.Cdf.chisq_P ~x:x ~nu
  | Logistic a              -> Gsl.Cdf.logistic_P ~x:x ~a
  | Gumbel1 (a, b)          -> Gsl.Cdf.gumbel1_P ~x:x ~a ~b
  | Gumbel2 (a, b)          -> Gsl.Cdf.gumbel2_P ~x:x ~a ~b
  | Rayleigh sigma          -> Gsl.Cdf.rayleigh_P ~x:x ~sigma
  | Exppow (a, b)           -> Gsl.Cdf.exppow_P ~x:x ~a ~b

(** [cdistr_sample dist] draws a random sample from the given continuous 
    distribution [dist]. It uses the GSL random number generator [rng]
    and dispatches to the corresponding GSL sampling function.
    For Uniform(lo, hi) where lo = hi, it returns lo directly.
    For Gaussian(mu, sigma), it adds mu to a sample from N(0, sigma).
    For Exponential, GSL's `exponential` uses `mu` (mean), which is `1.0 / lambda` (rate).
*)
let cdistr_sample dist =
  match dist with
  | Uniform (lo, hi) ->
      if lo = hi then lo else Gsl.Randist.flat rng ~a:lo ~b:hi
  | Gaussian (mu, sigma) ->
      mu +. Gsl.Randist.gaussian rng ~sigma
  | Exponential lambda ->
      Gsl.Randist.exponential rng ~mu:(1.0 /. lambda)
  | Beta (alpha, beta_param) ->
      Gsl.Randist.beta rng ~a:alpha ~b:beta_param
  | LogNormal (mu, sigma) ->
      Gsl.Randist.lognormal rng ~zeta:mu ~sigma
  | Gamma (a, b) ->
      Gsl.Randist.gamma rng ~a ~b
  | Laplace a ->
      Gsl.Randist.laplace rng ~a
  | Cauchy a ->
      Gsl.Randist.cauchy rng ~a
  | Pareto (a, b) ->
      Gsl.Randist.pareto rng ~a ~b
  | Weibull (a, b) ->
      Gsl.Randist.weibull rng ~a ~b
  | TDist nu ->
      Gsl.Randist.tdist rng ~nu
  | Chi2 nu ->
      Gsl.Randist.chisq rng ~nu
  | Logistic a ->
      Gsl.Randist.logistic rng ~a
  | Gumbel1 (a, b) ->
      Gsl.Randist.gumbel1 rng ~a ~b
  | Gumbel2 (a, b) ->
      Gsl.Randist.gumbel2 rng ~a ~b
  | Rayleigh sigma ->
      Gsl.Randist.rayleigh rng ~sigma
  | Exppow (a, b) ->
      Gsl.Randist.exppow rng ~a ~b

(* --- Helper Functions with Parameter Validation --- *)

(** [string_of_single_arg_dist_kind kind] converts a single-argument distribution 
    kind (from [Types.single_arg_dist_kind]) to its string representation.
    Useful for error messages or debugging. *)
let string_of_single_arg_dist_kind (kind: Types.single_arg_dist_kind) : string =
  match kind with
  | Types.DExponential -> "exponential"
  | Types.DLaplace     -> "laplace"
  | Types.DCauchy      -> "cauchy"
  | Types.DTDist       -> "tdist"
  | Types.DChi2        -> "chi2"
  | Types.DLogistic    -> "logistic"
  | Types.DRayleigh    -> "rayleigh"

(** [string_of_two_arg_dist_kind kind] converts a two-argument distribution 
    kind (from [Types.two_arg_dist_kind]) to its string representation.
    Useful for error messages or debugging. *)
let string_of_two_arg_dist_kind (kind: Types.two_arg_dist_kind) : string =
  match kind with
  | Types.DUniform     -> "uniform"
  | Types.DGaussian    -> "gaussian"
  | Types.DBeta        -> "beta"
  | Types.DLogNormal   -> "lognormal"
  | Types.DGamma       -> "gamma"
  | Types.DPareto      -> "pareto"
  | Types.DWeibull     -> "weibull"
  | Types.DGumbel1     -> "gumbel1"
  | Types.DGumbel2     -> "gumbel2"
  | Types.DExppow      -> "exppow"

(** [get_cdistr_from_single_arg_kind kind arg1] attempts to create a [cdistr]
    of the specified [kind] using parameter [arg1].
    It performs validation on [arg1] based on the distribution kind.
    Returns [Ok cdistr] on success, or [Error message] on validation failure. *)
let get_cdistr_from_single_arg_kind (kind: Types.single_arg_dist_kind) (arg1: float) : (cdistr, string) result =
  match kind with
  | Types.DExponential -> 
      if arg1 <= 0.0 then Error "Exponential rate (lambda) parameter must be positive" (* rate = arg1 *)
      else Ok (Exponential arg1)
  | Types.DLaplace -> 
      if arg1 <= 0.0 then Error "Laplace scale (b) parameter must be positive" (* scale = arg1 *)
      else Ok (Laplace arg1)
  | Types.DCauchy -> 
      if arg1 <= 0.0 then Error "Cauchy scale (gamma) parameter must be positive" (* scale = arg1 *)
      else Ok (Cauchy arg1)
  | Types.DTDist -> 
      if arg1 <= 0.0 then Error "TDist degrees of freedom (nu) parameter must be positive" (* nu = arg1 *)
      else Ok (TDist arg1)
  | Types.DChi2 -> 
      if arg1 <= 0.0 then Error "Chi2 degrees of freedom (nu) parameter must be positive" (* nu = arg1 *)
      else Ok (Chi2 arg1)
  | Types.DLogistic -> 
      if arg1 <= 0.0 then Error "Logistic scale (s) parameter must be positive" (* scale = arg1 *)
      else Ok (Logistic arg1)
  | Types.DRayleigh -> 
      if arg1 <= 0.0 then Error "Rayleigh scale (sigma) parameter must be positive" (* sigma = arg1 *)
      else Ok (Rayleigh arg1)

(** [get_cdistr_from_two_arg_kind kind arg1 arg2] attempts to create a [cdistr]
    of the specified [kind] using parameters [arg1] and [arg2].
    It performs validation on parameters based on the distribution kind.
    Returns [Ok cdistr] on success, or [Error message] on validation failure. *)
let get_cdistr_from_two_arg_kind (kind: Types.two_arg_dist_kind) (arg1: float) (arg2: float) : (cdistr, string) result =
  match kind with
  | Types.DUniform -> 
      if arg1 > arg2 then Error "Uniform low parameter must be less than or equal to high parameter" (* low = arg1, high = arg2 *)
      else Ok (Uniform (arg1, arg2))
  | Types.DGaussian -> 
      if arg2 <= 0.0 then Error "Gaussian standard deviation (sigma) parameter must be positive" (* mean = arg1, sigma = arg2 *)
      else Ok (Gaussian (arg1, arg2))
  | Types.DBeta -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Beta alpha and beta parameters must both be positive" (* alpha = arg1, beta = arg2 *)
      else Ok (Beta (arg1, arg2))
  | Types.DLogNormal -> 
      if arg2 <= 0.0 then Error "LogNormal sigma (scale of underlying Normal) parameter must be positive" (* mu = arg1, sigma = arg2 *)
      else Ok (LogNormal (arg1, arg2))
  | Types.DGamma -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Gamma shape and scale parameters must both be positive" (* shape = arg1, scale = arg2 *)
      else Ok (Gamma (arg1, arg2))
  | Types.DPareto -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Pareto scale (xm) and shape (alpha) parameters must both be positive" (* xm = arg1, alpha = arg2 *)
      else Ok (Pareto (arg1, arg2))
  | Types.DWeibull -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Weibull shape (k) and scale (lambda) parameters must both be positive" (* k = arg1, lambda = arg2 *)
      else Ok (Weibull (arg1, arg2))
  | Types.DGumbel1 -> 
      if arg2 <= 0.0 then Error "Gumbel1 scale (beta) parameter must be positive" (* location_mu = arg1, scale_beta = arg2 *)
      else Ok (Gumbel1 (arg1, arg2))
  | Types.DGumbel2 -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Gumbel2 shape (a) and scale (b) parameters must both be positive" (* shape_a = arg1, scale_b = arg2 *)
      else Ok (Gumbel2 (arg1, arg2))
  | Types.DExppow -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Exppow shape (kappa) and scale (mu) parameters must both be positive" (* kappa = arg1, mu = arg2 *)
      else Ok (Exppow (arg1, arg2))
