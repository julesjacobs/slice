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
  match dist with
  | Uniform (lo, hi)        ->
      if lo > hi then invalid_arg "Uniform CDF: lo must be ≤ hi"
      else Gsl.Cdf.flat_P ~x:x ~a:lo ~b:hi
  | Gaussian (m, s)         ->
      if s <= 0.0 then invalid_arg "Gaussian CDF: σ must be > 0";
      Gsl.Cdf.gaussian_P ~x:(x -. m) ~sigma:s
  | Exponential lambda      ->
      if lambda <= 0.0 then invalid_arg "Exponential CDF: λ must be > 0";
      Gsl.Cdf.exponential_P ~x:x ~mu:(1.0 /. lambda)
  | Beta (alpha, beta_param)      ->
      if alpha <= 0.0 || beta_param <= 0.0 then
        invalid_arg "Beta CDF: α and β must be > 0"
      else Gsl.Cdf.beta_P ~x:x ~a:alpha ~b:beta_param
  | LogNormal (mu, sigma)   ->
      if sigma <= 0.0 then invalid_arg "LogNormal CDF: σ must be > 0";
      Gsl.Cdf.lognormal_P ~x:x ~zeta:mu ~sigma
  | Gamma (a, b) ->
      if a <= 0.0 || b <= 0.0 then invalid_arg "Gamma CDF: shape and scale must be > 0";
      Gsl.Cdf.gamma_P ~x:x ~a ~b
  | Laplace a ->
      if a <= 0.0 then invalid_arg "Laplace CDF: scale must be > 0";
      Gsl.Cdf.laplace_P ~x:x ~a
  | Cauchy a ->
      if a <= 0.0 then invalid_arg "Cauchy CDF: scale must be > 0";
      Gsl.Cdf.cauchy_P ~x:x ~a
  | Pareto (a, b) ->
      if a <= 0.0 || b <= 0.0 then invalid_arg "Pareto CDF: a and b must be > 0";
      Gsl.Cdf.pareto_P ~x:x ~a ~b
  | Weibull (a, b) ->
      if a <= 0.0 || b <= 0.0 then invalid_arg "Weibull CDF: a and b must be > 0";
      Gsl.Cdf.weibull_P ~x:x ~a ~b
  | TDist nu ->
      if nu <= 0.0 then invalid_arg "TDist CDF: nu must be > 0";
      Gsl.Cdf.tdist_P ~x:x ~nu
  | Chi2 nu ->
      if nu <= 0.0 then invalid_arg "Chi2 CDF: nu must be > 0";
      Gsl.Cdf.chisq_P ~x:x ~nu
  | Logistic a ->
      if a <= 0.0 then invalid_arg "Logistic CDF: scale must be > 0";
      Gsl.Cdf.logistic_P ~x:x ~a
  | Gumbel1 (a, b) ->
      if b <= 0.0 then invalid_arg "Gumbel1 CDF: b must be > 0";
      Gsl.Cdf.gumbel1_P ~x:x ~a ~b
  | Gumbel2 (a, b) ->
      if b <= 0.0 then invalid_arg "Gumbel2 CDF: b must be > 0";
      Gsl.Cdf.gumbel2_P ~x:x ~a ~b
  | Rayleigh sigma ->
      if sigma <= 0.0 then invalid_arg "Rayleigh CDF: sigma must be > 0";
      Gsl.Cdf.rayleigh_P ~x:x ~sigma
  | Exppow (a, b) ->
      if a <= 0.0 || b <= 0.0 then invalid_arg "Exppow CDF: a and b must be > 0";
      Gsl.Cdf.exppow_P ~x:x ~a ~b

let cdistr_sample dist =
  match dist with
  | Uniform (lo, hi) ->
      if lo > hi then invalid_arg "Uniform sample: lo must ≤ hi";
      if lo = hi then lo else Gsl.Randist.flat rng ~a:lo ~b:hi

  | Gaussian (mu, sigma) ->
      if sigma <= 0.0 then invalid_arg "Gaussian sample: σ must be > 0";
      mu +. Gsl.Randist.gaussian rng ~sigma

  | Exponential lambda ->
      if lambda <= 0.0 then invalid_arg "Exponential sample: lambda must be > 0";
      Gsl.Randist.exponential rng ~mu:(1.0 /. lambda)

  | Beta (alpha, beta_param) -> (* Renamed beta to beta_param *)
      if alpha <= 0.0 || beta_param <= 0.0 then
        invalid_arg "Beta sample: α and β must be > 0";
      Gsl.Randist.beta rng ~a:alpha ~b:beta_param (* GSL can sample Beta *)

  | LogNormal (mu, sigma) ->
      if sigma <= 0.0 then invalid_arg "LogNormal sample: σ must be > 0";
      Gsl.Randist.lognormal rng ~zeta:mu ~sigma

  | Gamma (a, b) ->
      if a <= 0.0 || b <= 0.0 then invalid_arg "Gamma sample: shape and scale must be > 0";
      Gsl.Randist.gamma rng ~a ~b

  | Laplace a ->
      if a <= 0.0 then invalid_arg "Laplace sample: scale must be > 0";
      Gsl.Randist.laplace rng ~a

  | Cauchy a ->
      if a <= 0.0 then invalid_arg "Cauchy sample: scale must be > 0";
      Gsl.Randist.cauchy rng ~a

  | Pareto (a, b) ->
      if a <= 0.0 || b <= 0.0 then invalid_arg "Pareto sample: a and b must be > 0";
      Gsl.Randist.pareto rng ~a ~b

  | Weibull (a, b) ->
      if a <= 0.0 || b <= 0.0 then invalid_arg "Weibull sample: a and b must be > 0";
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
      if a <= 0.0 || b <= 0.0 then invalid_arg "Exppow sample: a and b must be > 0";
      Gsl.Randist.exppow rng ~a ~b

