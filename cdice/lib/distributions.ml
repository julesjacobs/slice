(** Continuous probability distributions and basic functionality            *)
(** No external libraries are used – only the OCaml standard library.       *)

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

(* ----------------------------------------------------------------------- *)
(*  Math helpers                                                            *)
(* ----------------------------------------------------------------------- *)

(** Error Function (erf) approximation – Abramowitz & Stegun 7.1.26 *)
let erf z =
  let p  = 0.327_591_1
  and a1 = 0.254_829_592
  and a2 = -.0.284_496_736
  and a3 = 1.421_413_741
  and a4 = -.1.453_152_027
  and a5 = 1.061_405_429
  in
  let sign  = if z < 0.0 then -.1.0 else 1.0 in
  let abs_z = abs_float z in
  let t     = 1.0 /. (1.0 +. p *. abs_z) in
  let term =
    t *. (a1 +. t *. (a2 +. t *. (a3 +. t *. (a4 +. t *. a5))))
  in
  let erf_val = 1.0 -. term *. exp (-. abs_z *. abs_z) in
  sign *. erf_val

(* The log-gamma function is often more numerically stable *)
let rec lanczos_lngamma z =
  let p = [|
    0.99999999999980993;
    676.5203681218851;
   -1259.1392167224028;
    771.32342877765313;
   -176.61502916214059;
    12.507343278686905;
    -0.13857109526572012;
    9.9843695780195716e-6;
    1.5056327351493116e-7;
  |]
  and g = 7.0 in
  if z < 0.5 then
    log Float.pi -. log (sin (Float.pi *. z)) -. lanczos_lngamma (1.0 -. z)
  else
    let z' = z -. 1.0 in
    let x = ref p.(0) in
    for i = 1 to Array.length p - 1 do
      x := !x +. p.(i) /. (z' +. float_of_int i)
    done;
    let t = z' +. g +. 0.5 in
    0.5 *. log (2.0 *. Float.pi) +. (z' +. 0.5) *. log t -. t +. log !x

(* Beta function B(a,b) = Γ(a)Γ(b) / Γ(a+b) *)
(* Using log-gamma for stability: ln(B(a,b)) = lnΓ(a) + lnΓ(b) - lnΓ(a+b) *)
let log_beta_function a b =
  lanczos_lngamma a +. lanczos_lngamma b -. lanczos_lngamma (a +. b)

(* ----------------------------------------------------------------------- *)
(*  PDFs for the distributions (moved before CDFs that might integrate them) *)
(* ----------------------------------------------------------------------- *)

let uniform_pdf ~lo ~hi x =
  if lo > hi then invalid_arg "uniform_pdf: lo must be ≤ hi"
  else if lo = hi then
    if x = lo then infinity
    else 0.0
  else if x >= lo && x <= hi then 1.0 /. (hi -. lo)
  else 0.0

let gaussian_pdf ~mu ~sigma x =
  if sigma <= 0.0 then invalid_arg "gaussian_pdf: σ must be > 0";
  let norm_const = 1.0 /. (sigma *. sqrt (2.0 *. Float.pi)) in
  let exponent = -0.5 *. ((x -. mu) /. sigma) ** 2.0 in
  norm_const *. exp exponent

let exponential_pdf ~lambda x =
  if lambda <= 0.0 then invalid_arg "exponential_pdf: λ must be > 0";
  if x < 0.0 then 0.0 else lambda *. exp (-. lambda *. x)

let beta_pdf ~a ~b x = (* Depends on log_beta_function, which uses lanczos_lngamma *)
  if a <= 0.0 || b <= 0.0 then
    invalid_arg "beta_pdf: α and β must be > 0";
  if x < 0.0 || x > 1.0 then 0.0
  else if x = 0.0 then
    if a < 1.0 then infinity
    else if a > 1.0 then 0.0
    else if b = 1.0 then 1.0
    else nan
  else if x = 1.0 then
    if b < 1.0 then infinity
    else if b > 1.0 then 0.0
    else if a = 1.0 then 1.0
    else nan
  else
    let log_pdf =
      (a -. 1.0) *. log x +. (b -. 1.0) *. log (1.0 -. x)
      -. log_beta_function a b
    in
    exp log_pdf

let lognormal_pdf ~mu ~sigma x =
  if sigma <= 0.0 then invalid_arg "lognormal_pdf: σ must be > 0";
  if x <= 0.0 then 0.0
  else
    let norm_const = 1.0 /. (x *. sigma *. sqrt (2.0 *. Float.pi)) in
    let exponent = -. ((log x -. mu) ** 2.0) /. (2.0 *. sigma *. sigma) in
    norm_const *. exp exponent

(* PDF Dispatcher *)
let cdistr_pdf dist x =
  match dist with
  | Uniform (lo, hi)        -> uniform_pdf ~lo ~hi x
  | Gaussian (m, s)         -> gaussian_pdf ~mu:m ~sigma:s x
  | Exponential lambda      -> exponential_pdf ~lambda x
  | Beta (alpha, beta)      -> beta_pdf ~a:alpha ~b:beta x
  | LogNormal (mu, sigma)   -> lognormal_pdf ~mu ~sigma x

(* ----------------------------------------------------------------------- *)
(*  Special-function utility for Beta CDF                                  *)
(* ----------------------------------------------------------------------- *)

(* Regularised incomplete beta I_x(a,b) via continued fraction - (used by beta_cdf) *)
let regularised_incomplete_beta ~a ~b ~x =
  if x < 0.0 || x > 1.0 then invalid_arg "I_x(a,b): x ∉ [0,1]";
  if x = 0.0 || x = 1.0 then x
  else
    let eps    = 1e-12 in
    let fpmine = 1e-30 in
    let betacf a b x =
      let qab = a +. b in
      let qap = a +. 1.0 in
      let qam = a -. 1.0 in
      let mutable_c = ref 1.0 in
      let d0 = 1.0 -. qab *. x /. qap in
      let mutable_d = ref (if abs_float d0 < fpmine then fpmine else d0) in
      mutable_d := 1.0 /. !mutable_d;
      let h = ref !mutable_d in
      let rec loop m =
        if m > 200 then !h
        else
          let m2 = 2 * m in
          let aa = (float_of_int m *. (b -. float_of_int m) *. x)
                   /. ((qam +. float_of_int m2) *. (a +. float_of_int m2)) in
          let d_even = 1.0 +. aa *. !mutable_d in
          mutable_d := 1.0 /. (if abs_float d_even < fpmine then fpmine else d_even);
          let c_val = 1.0 +. aa /. !mutable_c in
          mutable_c := if abs_float c_val < fpmine then fpmine else c_val;
          h := !h *. !mutable_d *. !mutable_c;
          let aa = ( -. (a +. float_of_int m) *. (qab +. float_of_int m) *. x)
                   /. ((a +. float_of_int m2) *. (qap +. float_of_int m2)) in
          let d_odd = 1.0 +. aa *. !mutable_d in
          mutable_d := 1.0 /. (if abs_float d_odd < fpmine then fpmine else d_odd);
          let c_val = 1.0 +. aa /. !mutable_c in
          mutable_c := if abs_float c_val < fpmine then fpmine else c_val;
          let del = !mutable_d *. !mutable_c in
          h := !h *. del;
          if abs_float (del -. 1.0) < eps then !h else loop (m + 1)
      in
      loop 1
    in
    let ln_pref = a *. log x +. b *. log (1.0 -. x) -. log_beta_function a b in
    let pref = exp ln_pref in
    if x < (a +. 1.0) /. (a +. b +. 2.0) then
      pref *. betacf a b x /. a
    else
      1.0 -. (pref *. betacf b a (1.0 -. x) /. b)

(* ----------------------------------------------------------------------- *)
(*  CDFs for all distributions                                              *)
(* ----------------------------------------------------------------------- *)

let uniform_cdf ~lo ~hi x =
  if lo > hi then invalid_arg "uniform_cdf: lo must be ≤ hi"
  else if lo = hi then
    if x < lo then 0.0 else 1.0
  else if x < lo then 0.0
  else if x >= hi then 1.0
  else (x -. lo) /. (hi -. lo)

let gaussian_cdf ~mu ~sigma x =
  if sigma <= 0.0 then invalid_arg "gaussian_cdf: σ must be > 0";
  let z = (x -. mu) /. (sigma *. sqrt 2.0) in
  0.5 *. (1.0 +. erf z)

let exponential_cdf ~lambda x =
  if lambda <= 0.0 then invalid_arg "exponential_cdf: λ must be > 0";
  if x < 0.0 then 0.0 else 1.0 -. exp (-. lambda *. x)

let beta_cdf ~a ~b x = (* Uses regularised_incomplete_beta *)
  if a <= 0.0 || b <= 0.0 then
    invalid_arg "beta_cdf: α and β must be > 0";
  if x <= 0.0 then 0.0
  else if x >= 1.0 then 1.0
  else regularised_incomplete_beta ~a ~b ~x

let lognormal_cdf ~mu ~sigma x =
  if sigma <= 0.0 then invalid_arg "lognormal_cdf: σ must be > 0";
  if x <= 0.0 then 0.0
  else
    let z = (log x -. mu) /. sigma in
    gaussian_cdf ~mu:0.0 ~sigma:1.0 z

(* CDF Dispatcher *)
let cdistr_cdf dist x =
  match dist with
  | Uniform (lo, hi)        -> uniform_cdf ~lo ~hi x
  | Gaussian (m, s)         -> gaussian_cdf ~mu:m ~sigma:s x
  | Exponential lambda      -> exponential_cdf ~lambda x
  | Beta (alpha, beta)      -> beta_cdf ~a:alpha ~b:beta x
  | LogNormal (mu, sigma)   -> lognormal_cdf ~mu ~sigma x

(* ----------------------------------------------------------------------- *)
(*  Random-number helpers                                                  *)
(* ----------------------------------------------------------------------- *)

(* Draw U ∈ (0,1), never 0.0 *)
let rec uniform_open () =
  let u = Random.float 1.0 in
  if u = 0.0 then uniform_open () else u

(* Standard normal via Box–Muller (returns one sample) *)
let standard_normal () =
  let u1 = uniform_open () in
  let u2 = Random.float 1.0 in
  sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2)

(* ----------------------------------------------------------------------- *)
(*  Main sampling function                                                  *)
(* ----------------------------------------------------------------------- *)

let cdistr_sample dist =
  match dist with
  | Uniform (lo, hi) ->
      if lo > hi then invalid_arg "Uniform: lo must ≤ hi";
      if lo = hi then lo else Random.float (hi -. lo) +. lo

  | Gaussian (mu, sigma) ->
      if sigma <= 0.0 then invalid_arg "Gaussian: σ must be > 0";
      mu +. sigma *. standard_normal ()

  | Exponential lambda ->
      if lambda <= 0.0 then invalid_arg "Exponential: lambda must be > 0";
      -.log (uniform_open ()) /. lambda

  | Beta (alpha, beta) ->
      if alpha <= 0.0 || beta <= 0.0 then
        invalid_arg "Beta: α and β must be > 0";
      invalid_arg "Beta sampling currently unavailable as it requires Gamma sampling."

  | LogNormal (mu, sigma) ->
      if sigma <= 0.0 then invalid_arg "LogNormal: σ must be > 0";
      exp (mu +. sigma *. standard_normal ())

(* ----------------------------------------------------------------------- *)
(*  Example usage                                                           *)
(* ----------------------------------------------------------------------- *)
(*
let () =
  Random.self_init ();
  let d = Gaussian (0.0, 1.0) in
  for i = 1 to 5 do
    Printf.printf "%f\n" (cdistr_sample d)
  done
*)
