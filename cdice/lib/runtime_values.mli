(* Runtime values and environment *)

type expr = Ast.expr (* From the AST module *)
and env_entry = (string * value)
and env = env_entry list

and value =
  | VBool of bool
  | VFloat of float
  | VPair of value * value
  | VFin of int * int (* value k, modulus n *)
  | VClosure of string * expr * env (* string is var name, Ast.expr is body, env is captured env *)
  | VUnit
  | VNil (* Runtime value for nil *)
  | VCons of value * value (* Runtime value for cons *)
  | VRef of value ref (* Runtime value for references *)

(** Helper for pretty printing values *)
val string_of_value : value -> string
