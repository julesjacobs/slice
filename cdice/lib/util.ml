(**
 * @file util.ml
 * @brief General utility functions for the ContDice compiler.
 *
 * This module provides helper functions used across different parts of the
 * ContDice system, such as generating fresh variable names, constructing
 * let-bindings in the AST, and calculating bit lengths.
 *)

open Types (* To get Ast module alias *)
open Ast (* To get expr, ExprNode, Var, Let directly *)

(** Counter for generating fresh variable names to avoid collisions. *)
let var_counter = ref 0

(**
 * @function fresh_var
 * @brief Generates a fresh variable name with a given prefix.
 * Appends an incrementing integer to the prefix.
 *
 * @param prefix The string prefix for the new variable name (e.g., "_let_var").
 * @return A new, unique variable name (e.g., "_let_var1", "_let_var2").
 *)
let fresh_var (prefix : string) : string =
  incr var_counter; (* Increment the global counter *)
  prefix ^ string_of_int !var_counter (* Append current counter value to prefix *)

(**
 * @function gen_let
 * @brief Smart constructor for AST `Let` nodes.
 *
 * If the right-hand side expression (`rhs_expr`) is already a simple variable,
 * it avoids creating an unnecessary `Let` binding and directly calls the `body_fn`
 * with the existing variable name. Otherwise, it generates a fresh variable name,
 * creates a `Let` node binding this fresh variable to `rhs_expr`, and then
 * uses this fresh variable in the body created by `body_fn`.
 *
 * @param base_name_hint A hint for the prefix of the fresh variable if one needs to be generated.
 * @param rhs_expr The expression for the right-hand side of the let binding.
 * @param body_fn A function that takes the variable name (either existing or fresh)
 *                and returns the body expression of the let binding.
 * @return An `Ast.expr` which is either the body itself (if `rhs_expr` was a var) or a new `Let` node.
 *)
let gen_let (base_name_hint : string) (rhs_expr : expr)
    (body_fn : string -> expr) : expr =
  match rhs_expr with
  | ExprNode (Var existing_var_name) ->
      (* If rhs is already a variable, don't create a new let; just use the existing variable in the body. *)
      body_fn existing_var_name
  | _ ->
      (* Otherwise, create a fresh variable for the rhs. *)
      let new_var_name = fresh_var base_name_hint in
      ExprNode (Let (new_var_name, rhs_expr, body_fn new_var_name))

(**
 * @function bit_length
 * @brief Calculates the number of bits required to represent a non-negative integer.
 *
 * @param n The non-negative integer.
 * @return The minimum number of bits to represent `n`. Returns 1 for `n = 0`.
 * @raise Invalid_argument if `n` is negative.
 *)
let bit_length n =
  if n < 0 then invalid_arg "bit_length: only non-negative integers allowed"
  else if n = 0 then 1 (* Special case for 0, requires 1 bit. *)
  else
    let rec aux num acc =
      if num = 0 then acc else aux (num lsr 1) (acc + 1) (* Right shift and increment count *)
    in
    aux n 0

(**
 * @var curr_max_int_sz
 * @brief A mutable reference holding the current maximum integer size (in bits)
 * encountered or required during certain operations, like `to_dice` conversion.
 * This can be used to determine a global or dynamic bit width for integer types
 * in a target language if fixed-width integers are needed.
 * Initialized to 0.
 *)
let curr_max_int_sz = ref 0
