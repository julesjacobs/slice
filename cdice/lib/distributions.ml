(** Continuous probability distributions and basic functionality            *)

(* ----------------------------------------------------------------------- *)
(*  Type definition                                                         *)
(* ----------------------------------------------------------------------- *)

(** The type representing continuous distributions *)
type cdistr =
  | Uniform   of float * float            (** Uniform(low, high)                    *)
  | Gaussian  of float * float            (** Gaussian(mean, std_dev)               *)
  | Exponential of float                  (** Exponential(rate_lambda)              *)
  | Beta     of float * float             (** Beta(alpha, beta)                     *)
  | LogNormal of float * float            (** LogNormal(mu, sigma) – ln-Normal      *)
  | Gamma    of float * float             (** Gamma(shape, scale)                   *)
  | Laplace  of float                     (** Laplace(scale)                        *)
  | Cauchy   of float                     (** Cauchy(scale)                         *)
  | Pareto   of float * float             (** Pareto(a, b)                          *)
  | Weibull  of float * float             (** Weibull(a, b)                         *)
  | TDist    of float                     (** Student's t (nu)                      *)
  | Chi2     of float                     (** Chi-squared (nu)                      *)
  | Logistic of float                     (** Logistic(scale)                       *)
  | Gumbel1  of float * float             (** Gumbel1(a, b)                         *)
  | Gumbel2  of float * float             (** Gumbel2(a, b)                         *)
  | Rayleigh of float                     (** Rayleigh(sigma)                       *)
  | Exppow   of float * float             (** Exponential Power (a, b)              *)

(* Initialize GSL random number generator *)
let rng = Gsl.Rng.make (Gsl.Rng.default ())

let cdistr_cdf dist x =
  if x = neg_infinity then 0.0
  else if x = infinity then 1.0
  else
    match dist with
  | Uniform (lo, hi)        -> Gsl.Cdf.flat_P ~x:x ~a:lo ~b:hi
  | Gaussian (m, s)         -> Gsl.Cdf.gaussian_P ~x:(x -. m) ~sigma:s
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

let string_of_single_arg_dist_kind (kind: Types.single_arg_dist_kind) : string =
  match kind with
  | Types.DExponential -> "exponential"
  | Types.DLaplace     -> "laplace"
  | Types.DCauchy      -> "cauchy"
  | Types.DTDist       -> "tdist"
  | Types.DChi2        -> "chi2"
  | Types.DLogistic    -> "logistic"
  | Types.DRayleigh    -> "rayleigh"

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

let get_cdistr_from_single_arg_kind (kind: Types.single_arg_dist_kind) (arg1: float) : (cdistr, string) result =
  match kind with
  | Types.DExponential -> 
      if arg1 <= 0.0 then Error "Exponential lambda must be positive"
      else Ok (Exponential arg1)
  | Types.DLaplace -> 
      if arg1 <= 0.0 then Error "Laplace scale must be positive"
      else Ok (Laplace arg1)
  | Types.DCauchy -> 
      if arg1 <= 0.0 then Error "Cauchy scale must be positive"
      else Ok (Cauchy arg1)
  | Types.DTDist -> 
      if arg1 <= 0.0 then Error "TDist nu must be positive"
      else Ok (TDist arg1)
  | Types.DChi2 -> 
      if arg1 <= 0.0 then Error "Chi2 nu must be positive"
      else Ok (Chi2 arg1)
  | Types.DLogistic -> 
      if arg1 <= 0.0 then Error "Logistic scale must be positive"
      else Ok (Logistic arg1)
  | Types.DRayleigh -> 
      if arg1 <= 0.0 then Error "Rayleigh sigma must be positive"
      else Ok (Rayleigh arg1)

let get_cdistr_from_two_arg_kind (kind: Types.two_arg_dist_kind) (arg1: float) (arg2: float) : (cdistr, string) result =
  match kind with
  | Types.DUniform -> 
      if arg1 > arg2 then Error "Uniform low > high"
      else Ok (Uniform (arg1, arg2))
  | Types.DGaussian -> 
      if arg2 <= 0.0 then Error "Gaussian sigma must be positive"
      else Ok (Gaussian (arg1, arg2))
  | Types.DBeta -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Beta alpha and beta_param must be positive"
      else Ok (Beta (arg1, arg2))
  | Types.DLogNormal -> 
      if arg2 <= 0.0 then Error "LogNormal sigma must be positive"
      else Ok (LogNormal (arg1, arg2))
  | Types.DGamma -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Gamma shape and scale must be positive"
      else Ok (Gamma (arg1, arg2))
  | Types.DPareto -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Pareto a and b must be positive"
      else Ok (Pareto (arg1, arg2))
  | Types.DWeibull -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Weibull a and b must be positive"
      else Ok (Weibull (arg1, arg2))
  | Types.DGumbel1 -> 
      if arg2 <= 0.0 then Error "Gumbel1 b must be positive"
      else Ok (Gumbel1 (arg1, arg2))
  | Types.DGumbel2 -> 
      if arg2 <= 0.0 then Error "Gumbel2 b must be positive"
      else Ok (Gumbel2 (arg1, arg2))
  | Types.DExppow -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Exppow a and b must be positive"
      else Ok (Exppow (arg1, arg2))
