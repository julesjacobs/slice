(** Continuous probability distributions and basic functionality            *)

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
  | Poisson   of float                    (** Poisson(mu)                           *)
  | Binomial   of float * int             (** Binomial(p, n)                        *)

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
  | Poisson mu              -> Gsl.Cdf.poisson_P ~k:(int_of_float x) ~mu
  | Binomial (p, n)         -> Gsl.Cdf.binomial_P ~k:(int_of_float x) ~p ~n

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
  | Poisson mu ->
      float_of_int (Gsl.Randist.poisson rng ~mu)
  | Binomial (p,n) ->
      float_of_int (Gsl.Randist.binomial rng ~p ~n)

let string_of_single_arg_dist_kind (kind: Ast.single_arg_dist_kind) : string =
  match kind with
  | Ast.DExponential -> "exponential"
  | Ast.DLaplace     -> "laplace"
  | Ast.DCauchy      -> "cauchy"
  | Ast.DTDist       -> "tdist"
  | Ast.DChi2        -> "chi2"
  | Ast.DLogistic    -> "logistic"
  | Ast.DRayleigh    -> "rayleigh"
  | Ast.DPoisson     -> "poisson"

let string_of_two_arg_dist_kind (kind: Ast.two_arg_dist_kind) : string =
  match kind with
  | Ast.DUniform     -> "uniform"
  | Ast.DGaussian    -> "gaussian"
  | Ast.DBeta        -> "beta"
  | Ast.DLogNormal   -> "lognormal"
  | Ast.DGamma       -> "gamma"
  | Ast.DPareto      -> "pareto"
  | Ast.DWeibull     -> "weibull"
  | Ast.DGumbel1     -> "gumbel1"
  | Ast.DGumbel2     -> "gumbel2"
  | Ast.DExppow      -> "exppow"
  | Ast.DBinomial    -> "binomial"

let get_cdistr_from_single_arg_kind (kind: Ast.single_arg_dist_kind) (arg1: float) : (cdistr, string) result =
  match kind with
  | Ast.DExponential -> 
      if arg1 <= 0.0 then Error "Exponential lambda must be positive"
      else Ok (Exponential arg1)
  | Ast.DLaplace -> 
      if arg1 <= 0.0 then Error "Laplace scale must be positive"
      else Ok (Laplace arg1)
  | Ast.DCauchy -> 
      if arg1 <= 0.0 then Error "Cauchy scale must be positive"
      else Ok (Cauchy arg1)
  | Ast.DTDist -> 
      if arg1 <= 0.0 then Error "TDist nu must be positive"
      else Ok (TDist arg1)
  | Ast.DChi2 -> 
      if arg1 <= 0.0 then Error "Chi2 nu must be positive"
      else Ok (Chi2 arg1)
  | Ast.DLogistic -> 
      if arg1 <= 0.0 then Error "Logistic scale must be positive"
      else Ok (Logistic arg1)
  | Ast.DRayleigh -> 
      if arg1 <= 0.0 then Error "Rayleigh sigma must be positive"
      else Ok (Rayleigh arg1)
  | Ast.DPoisson -> 
      if arg1 <= 0.0 then Error "Poisson mu must be positive"
      else Ok (Poisson arg1)

let get_cdistr_from_two_arg_kind (kind: Ast.two_arg_dist_kind) (arg1: float) (arg2: float) : (cdistr, string) result =
  match kind with
  | Ast.DUniform -> 
      if arg1 > arg2 then Error "Uniform low > high"
      else Ok (Uniform (arg1, arg2))
  | Ast.DGaussian -> 
      if arg2 <= 0.0 then Error "Gaussian sigma must be positive"
      else Ok (Gaussian (arg1, arg2))
  | Ast.DBeta -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Beta alpha and beta_param must be positive"
      else Ok (Beta (arg1, arg2))
  | Ast.DLogNormal -> 
      if arg2 <= 0.0 then Error "LogNormal sigma must be positive"
      else Ok (LogNormal (arg1, arg2))
  | Ast.DGamma -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Gamma shape and scale must be positive"
      else Ok (Gamma (arg1, arg2))
  | Ast.DPareto -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Pareto a and b must be positive"
      else Ok (Pareto (arg1, arg2))
  | Ast.DWeibull -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Weibull a and b must be positive"
      else Ok (Weibull (arg1, arg2))
  | Ast.DGumbel1 -> 
      if arg2 <= 0.0 then Error "Gumbel1 b must be positive"
      else Ok (Gumbel1 (arg1, arg2))
  | Ast.DGumbel2 -> 
      if arg2 <= 0.0 then Error "Gumbel2 b must be positive"
      else Ok (Gumbel2 (arg1, arg2))
  | Ast.DExppow -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Exppow a and b must be positive"
      else Ok (Exppow (arg1, arg2))
  | Ast.DBinomial -> 
      if arg1 <= 0.0 || arg2 <= 0.0 then Error "Binomial a and b must be positive"
      else Ok (Binomial (arg1, int_of_float arg2))