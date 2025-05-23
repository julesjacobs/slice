(**
 * @file runtime_values.ml
 * @brief Defines the types for runtime values and environments in the ContDice interpreter.
 *
 * This module specifies the structure of values that expressions evaluate to (e.g.,
 * booleans, floats, closures, lists, references) and the environment that maps
 * variable names to these values. It also includes a helper function for converting
 * values to their string representations for display.
 *)

(** The type of expressions, imported from the Ast module.
    It's used here because closures capture expressions. *)
type expr = Ast.expr

(** An entry in the runtime environment, mapping a variable name (string) to a runtime value. *)
and env_entry = (string * value)

(** The runtime environment, represented as a list of (string * value) pairs.
    This is a simple association list where the most recent binding for a variable shadows older ones. *)
and env = env_entry list

(** Represents all possible values that an expression can evaluate to at runtime. *)
and value =
  | VBool of bool  (** Boolean value (e.g., `true`, `false`). *)
  | VFloat of float  (** Floating-point number (e.g., `3.14`). *)
  | VPair of value * value  (** A pair of two runtime values (e.g., `(v1, v2)`). *)
  | VFin of int * int  (** A finite set value `k#n`, where `k` is the value and `n` is the modulus. *)
  | VClosure of string * expr * env  (** A closure, representing a function. It captures the parameter name, function body (an `Ast.expr`), and the environment at the time of definition. *)
  | VUnit  (** The unit value, `()`. *)
  | VNil  (** The empty list value, `[]`. *)
  | VCons of value * value  (** A non-empty list, constructed with a head value and a tail list value (e.g., `v_head :: v_tail`). *)
  | VRef of value ref  (** A mutable reference cell containing a runtime value. *)

(**
 * @brief Converts a runtime value to its string representation.
 *
 * This function is recursive and handles all variants of the `value` type.
 * For lists, it attempts to print them in a standard OCaml-like format `[v1; v2; v3]`.
 *
 * @param v The value to convert to a string.
 * @return The string representation of the value.
 *)
let rec string_of_value = function
  | VBool b -> string_of_bool b
  | VFloat f -> string_of_float f
  | VPair (v1, v2) ->
      Printf.sprintf "(%s, %s)" (string_of_value v1) (string_of_value v2)
  | VFin (k, n) -> Printf.sprintf "%d#%d" k n
  | VClosure (x, _, _) -> Printf.sprintf "<fun %s>" x
  | VUnit -> "()"
  | VNil -> "[]"
  | VCons (v_hd, VNil) ->
      (* Special case for a list with a single element. *)
      Printf.sprintf "[%s]" (string_of_value v_hd)
  | VCons (v_hd, v_tl) when is_proper_list_for_printing v_tl ->
      (* If the tail is a proper list, print in [v_hd; ...tail...] format. *)
      Printf.sprintf "[%s%s]" (string_of_value v_hd) (string_of_list_tail v_tl)
  | VCons (v_hd, v_tl) ->
      (* General case for cons cells, not necessarily forming a standard list (e.g., (1 :: (2 :: 3)) vs (1 :: not_a_list)). *)
      Printf.sprintf "(%s :: %s)" (string_of_value v_hd) (string_of_value v_tl)
  | VRef v -> Printf.sprintf "ref(%s)" (string_of_value !v)

(**
 * @brief Checks if a value, when treated as a list tail, forms a "proper" list structure
 * suitable for printing in the `[v1; v2; ...]` format.
 *
 * A proper list tail is either `VNil` or a `VCons` whose own tail is proper.
 * This helps `string_of_value` decide on the printing format for `VCons`.
 *
 * @param v_tl The value to check if it's a proper list tail.
 * @return `true` if `v_tl` is `VNil` or a `VCons` leading to `VNil`, `false` otherwise.
 *)
and is_proper_list_for_printing = function
  | VNil -> true
  | VCons (_, VNil) -> true (* Base case for the end of a list being printed. *)
  | VCons (_, v_further_tl) -> is_proper_list_for_printing v_further_tl (* Recurse on the tail of the tail. *)
  | _ -> false (* Not a VNil or VCons, so not part of a standard list structure for printing. *)

(**
 * @brief Converts the tail of a list value to its string representation for use within `[...]`.
 *
 * This function is called by `string_of_value` when a `VCons` is part of a proper list.
 * It prepends each element with "; " and recursively calls `string_of_value` for elements
 * and `string_of_list_tail` for the rest of the tail.
 *
 * @param v_tl The tail of the list to convert.
 * @return The string representation of the list tail (e.g., `"; v2; v3"`).
 * @raises Failure if `v_tl` is not a `VNil` or `VCons` (should not happen if `is_proper_list_for_printing` is used correctly).
 *)
and string_of_list_tail = function
  | VNil -> "" (* End of the list. *)
  | VCons (v_hd, v_further_tl) ->
      Printf.sprintf "; %s%s" (string_of_value v_hd) (string_of_list_tail v_further_tl)
  | _ -> failwith "Invariant violation: string_of_list_tail called with non-list tail" (* Should not be reached if called after is_proper_list_for_printing. *)
