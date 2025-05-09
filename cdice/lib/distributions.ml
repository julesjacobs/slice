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

