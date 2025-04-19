(** Module for statistical distributions and their cumulative distribution functions *)

(** The type representing continuous distributions *)
type cdistr =
  | Uniform of float * float   (** Uniform(low, high) *)
  | Gaussian of float * float  (** Gaussian(mean, std_dev) *)
  | Exponential of float     (** Exponential(rate_lambda) *)
  | Beta of float * float      (** Beta(alpha, beta) *)

(** Calculate the Cumulative Distribution Function (CDF) for a given distribution and value *)
val cdistr_cdf : cdistr -> float -> float 