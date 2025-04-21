(** The type representing continuous distributions *)
type cdistr =
| Uniform of float * float   (** Uniform(low, high) *)
| Gaussian of float * float  (** Gaussian(mean, std_dev) *)
| Exponential of float     (** Exponential(rate_lambda) *)
| Beta of float * float      (** Beta(alpha, beta) *)

(* WARNING: the functionality below may be entirely incorrect *)

(** Internal helper: Error Function (erf) approximation *)
(* This uses the Abramowitz and Stegun formula 7.1.26, which is reasonably accurate. *)
(* erf(z) = (2 / sqrt(pi)) * integral from 0 to z of exp(-t^2) dt *)
let erf z =
    (* Constants for the approximation *)
    let p = 0.3275911 in
    let a1 = 0.254829592 in
    let a2 = -0.284496736 in
    let a3 = 1.421413741 in
    let a4 = -1.453152027 in
    let a5 = 1.061405429 in

    let sign = if z < 0.0 then -1.0 else 1.0 in
    let abs_z = abs_float z in

    (* A&S formula 7.1.26 *)
    let t = 1.0 /. (1.0 +. p *. abs_z) in
    let term = t *. (a1 +. t *. (a2 +. t *. (a3 +. t *. (a4 +. t *. a5)))) in
    let erf_val = 1.0 -. term *. exp (-. abs_z *. abs_z) in

sign *. erf_val

(** Cumulative Distribution Function (CDF) for Uniform distribution *)
let uniform_cdf ~lo ~hi x =
    if lo > hi then invalid_arg "uniform_cdf: lo must be less than hi"
    else if lo = hi then
        (* Handle degenerate case where lo = hi *)
        if x < lo then 0.0 else 1.0
    else if x < lo then 0.0
    else if x >= hi then 1.0
    else (x -. lo) /. (hi -. lo)

(** Cumulative Distribution Function (CDF) for Gaussian (Normal) distribution *)
(* CDF(x) = 0.5 * (1 + erf((x - mu) / (sigma * sqrt(2)))) *)
let gaussian_cdf ~mu ~sigma x =
    if sigma <= 0.0 then invalid_arg "gaussian_cdf: sigma must be positive"
    else
        let z = (x -. mu) /. (sigma *. sqrt 2.0) in
        0.5 *. (1.0 +. erf z)

(** Cumulative Distribution Function (CDF) for Exponential distribution *)
(* CDF(x) = 1 - exp(-lambda * x) for x >= 0 *)
let exponential_cdf ~lambda x =
    if lambda <= 0.0 then invalid_arg "exponential_cdf: lambda must be positive"
    else if x < 0.0 then 0.0
    else 1.0 -. exp (-. lambda *. x)

(** Cumulative Distribution Function (CDF) for Beta distribution *)
(* CDF(x) = I_x(a, b), the regularized incomplete beta function. *)
(* Implementing this accurately without external libraries is complex. *)
(* Common methods involve continued fractions or series expansions. *)
(* This is a placeholder implementation. *)
let beta_cdf ~a ~b x =
    if a <= 0.0 || b <= 0.0 then invalid_arg "beta_cdf: a and b must be positive"
    else if x <= 0.0 then 0.0 (* CDF is 0 at or below 0 *)
    else if x >= 1.0 then 1.0 (* CDF is 1 at or above 1 *)
    else
        (* Placeholder: A full implementation requires the Incomplete Beta Function. *)
        nan (* Return NaN for unimplemented function *)


(** Calculate the Cumulative Distribution Function (CDF) for a given distribution and value *)
let cdistr_cdf (dist : cdistr) (x : float) : float =
    match dist with
    | Uniform (lo, hi) -> uniform_cdf ~lo ~hi x
    | Gaussian (mean, std) -> gaussian_cdf ~mu:mean ~sigma:std x
    | Exponential rate -> exponential_cdf ~lambda:rate x
    | Beta (alpha, beta) -> beta_cdf ~a:alpha ~b:beta x

(** Sample from a continuous distribution *)
let cdistr_sample (dist : cdistr) : float =
    match dist with
    | Uniform (lo, hi) ->
        if lo > hi then invalid_arg "cdistr_sample: Uniform lo must be less than hi"
        else if lo = hi then lo (* Degenerate case *)
        else Random.float (hi -. lo) +. lo
    | Gaussian (mu, sigma) ->
        if sigma <= 0.0 then invalid_arg "cdistr_sample: Gaussian sigma must be positive";
        (* Box-Muller transform *)
        let u1 = ref 0.0 in
        let u2 = ref 0.0 in
        (* Ensure u1 is not 0 to avoid log(0) *)
        while !u1 = 0.0 do
          u1 := Random.float 1.0
        done;
        u2 := Random.float 1.0;
        let z0 = sqrt (-2.0 *. log !u1) *. cos (2.0 *. Float.pi *. !u2) in
        (* We only need one sample, z0 *)
        mu +. sigma *. z0
    | Exponential _ -> failwith "cdistr_sample: Exponential sampling not implemented"
    | Beta _ -> failwith "cdistr_sample: Beta sampling not implemented"