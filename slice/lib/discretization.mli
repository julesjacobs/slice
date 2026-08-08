val get_const_idx_and_modulus : float -> Lats.CutSet.t -> int * int

val interval_probabilities :
  Lats.CutSet.t -> Distributions.cdistr -> float list

val prob_cdistr_intervals : Lats.cut list -> Distributions.cdistr -> float list

val discretize : Ast.texpr -> Ast.expr

val discretize_top : Ast.texpr -> Ast.expr
