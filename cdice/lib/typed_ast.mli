(* Typed Abstract Syntax Tree (TAST) types *)

(** 'texpr' represents a typed expression. It's a pair of a type and an 'aexpr'.
    The type 'ty' comes from the TypeSystem module and represents the inferred
    or checked type of the expression. *)
type texpr = TypeSystem.ty * aexpr

(** 'aexpr' (annotated expression) is the recursive part of the typed AST.
    It uses the generic expression structure 'expr_generic' from the Ast module,
    but it's parameterized with 'texpr' itself. This means that sub-expressions
    within a typed expression are also typed expressions. *)
and aexpr = TAExprNode of texpr Ast.expr_generic
