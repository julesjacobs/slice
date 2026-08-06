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

let is_finite x =
  match classify_float x with
  | FP_normal | FP_subnormal | FP_zero -> true
  | FP_infinite | FP_nan -> false

let validate_uniform_bounds lo hi =
  if not (is_finite lo && is_finite hi) then
    Error "Uniform bounds must be finite"
  else if lo >= hi then
    Error "Uniform low must be less than high"
  else
    Ok ()

let validate_uniform_bounds_exn lo hi =
  match validate_uniform_bounds lo hi with
  | Ok () -> ()
  | Error msg -> invalid_arg msg

let uniform_cdf_interior lo hi x =
  let width = hi -. lo in
  if is_finite width then
    (x -. lo) /. width
  else
    (* Scaling avoids overflow in [hi - lo] when finite bounds span most of
       the floating-point range. *)
    let scale = max (abs_float lo) (abs_float hi) in
    ((x /. scale) -. (lo /. scale))
    /. ((hi /. scale) -. (lo /. scale))

(* Keep integer-support CDF calls inside the range representable by OCaml
   integers.  PR #15 further restricts this to the GSL unsigned-int range. *)
let integer_cdf ~upper_support cdf x =
  if classify_float x = FP_nan then invalid_arg "CDF is undefined at NaN"
  else if x < 0.0 then 0.0
  else
    match upper_support with
    | Some upper when x >= float_of_int upper -> 1.0
    | _ when x >= float_of_int max_int -> 1.0
    | _ -> cdf (int_of_float (floor x))

let cdistr_cdf dist x =
  if classify_float x = FP_nan then invalid_arg "CDF is undefined at NaN"
  else match dist with
  | Uniform (lo, hi) ->
      validate_uniform_bounds_exn lo hi;
      if x <= lo then 0.0
      else if x >= hi then 1.0
      else uniform_cdf_interior lo hi x
  | _ when x = neg_infinity -> 0.0
  | _ when x = infinity -> 1.0
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
  | Poisson mu              ->
      integer_cdf ~upper_support:None
        (fun k -> Gsl.Cdf.poisson_P ~k ~mu) x
  | Binomial (p, n)         ->
      integer_cdf ~upper_support:(Some n)
        (fun k -> Gsl.Cdf.binomial_P ~k ~p ~n) x

(** [cdistr_cdf_below dist x] is the left limit of the CDF: [P(X < x)].
    It differs from [cdistr_cdf] at atoms. Integer-valued distributions need
    an explicit implementation so that an endpoint's strictness is preserved. *)
let cdistr_cdf_below dist x =
  if classify_float x = FP_nan then invalid_arg "CDF is undefined at NaN"
  else if x = neg_infinity then 0.0
  else if x = infinity then 1.0
  else
    match dist with
    | Poisson _ | Binomial _ ->
        if x <= 0.0 then 0.0 else cdistr_cdf dist (ceil x -. 1.0)
    | _ -> cdistr_cdf dist x

let cdistr_sample dist =
  match dist with
  | Uniform (lo, hi) ->
      validate_uniform_bounds_exn lo hi;
      Gsl.Randist.flat rng ~a:lo ~b:hi
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
      (match validate_uniform_bounds arg1 arg2 with
       | Ok () -> Ok (Uniform (arg1, arg2))
       | Error _ as error -> error)
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
      if arg1 <= 0.0 || arg2 < 0.0 then Error "Binomial p must be positive and n must be non-negative"
      else Ok (Binomial (arg1, int_of_float arg2))
