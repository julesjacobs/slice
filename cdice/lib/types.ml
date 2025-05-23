(**
 * @file types.ml
 * @brief Main type aggregation module for the ContDice language.
 *
 * This module previously contained all core type definitions for the language.
 * As part of a refactoring effort to improve modularity, these definitions
 * have been moved into separate sub-modules:
 *
 * - `Ast`: Defines the Abstract Syntax Tree (AST) structures (e.g., `expr`, `expr_generic`).
 * - `TypeSystem`: Defines types related to the type system (e.g., `ty`, `meta_ref`) and unification.
 * - `TypedAst`: Defines the Typed Abstract Syntax Tree (TAST) structures (e.g., `texpr`, `aexpr`).
 * - `RuntimeValues`: Defines types for runtime values and environments (e.g., `value`, `env`).
 *
 * This `Types` module now acts as a convenient re-exporter of these sub-modules.
 * Code that previously did `open Types` can still access these types, typically
 * by qualifying them with the sub-module name (e.g., `Ast.expr`, `TypeSystem.ty`)
 * or by additionally opening the specific sub-module (e.g., `open Types.Ast`).
 *
 * For detailed definitions, please refer to the respective sub-modules.
 *)

(** Module for Abstract Syntax Tree definitions. See `ast.ml` and `ast.mli`. *)
module Ast = Ast

(** Module for Type System definitions (types, unification). See `type_system.ml` and `type_system.mli`. *)
module TypeSystem = TypeSystem

(** Module for Typed Abstract Syntax Tree definitions. See `typed_ast.ml` and `typed_ast.mli`. *)
module TypedAst = TypedAst

(** Module for Runtime Value and Environment definitions. See `runtime_values.ml` and `runtime_values.mli`. *)
module RuntimeValues = RuntimeValues

(*
  Note: If direct re-exporting of specific types from sub-modules into the
  `Types` namespace is desired for backward compatibility or convenience,
  it can be done here. For example:

  type expr = Ast.expr
  type ty = TypeSystem.ty
  type value = RuntimeValues.value
  type texpr = TypedAst.texpr

  However, the current approach encourages more explicit namespacing via
  sub-module access (e.g., Ast.expr) or opening specific sub-modules.
*)
