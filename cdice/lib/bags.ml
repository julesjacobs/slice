(**
 * @file bags.ml
 * @brief Defines specific Bag types for managing float constants and bounds in ContDice.
 *
 * This module builds upon the generic `Bag` functor by defining:
 * - `FloatSet`: A set of floats.
 * - `bound`: A type representing comparison bounds (e.g., `< c`, `<= c`).
 * - `BoundSet`: A set of such bounds.
 * - `FloatBag`: A Bag specialized to hold `FloatSet.t set_or_top` (either a finite set of floats or Top).
 *   Used for tracking possible constant float values an expression might evaluate to.
 * - `BoundBag`: A Bag specialized to hold `BoundSet.t set_or_top` (either a finite set of bounds or Top).
 *   Used for tracking comparison bounds an expression might be subject to.
 *
 * The `set_or_top` type allows representing either a concrete collection of values/bounds
 * or an unconstrained "Top" state. The `BoundBag` module is further extended to include
 * an `add_all` function for convenience.
 *)

open Bag (* Ensure this is at the top for access to the Bag.Make functor and Lat signature *)

(**
 * @type ('elt, 'set) set_or_top
 * @brief Represents a value that can either be a finite set of elements or a "Top" value.
 * "Top" typically signifies an unconstrained or universal set in a lattice context.
 *
 * @param 'elt The type of elements in the set.
 * @param 'set The type of the set itself (e.g., `FloatSet.t`).
 *)
type ('elt, 'set) set_or_top =
  | Finite of 'set (** Represents a specific, finite set of elements. *)
  | Top  (** Represents the "Top" element of a lattice, often meaning unconstrained or all possible values. *)

(* == Float Set == *)
(** Module defining a set of floats, using OCaml's `Set.Make`. *)
module FloatSet = Set.Make (struct
  type t = float (** Element type is float. *)
  let compare = compare (** Standard comparison for floats. *)
end)

(* == Bound Type == *)
(**
 * @type bound
 * @brief Represents a comparison bound involving a float constant.
 * Used to track constraints like `x < 5.0` or `y <= 3.0`.
 *)
type bound =
  | Less of float  (** Represents a strict less-than bound (e.g., `< c`). *)
  | LessEq of float  (** Represents a less-than-or-equal-to bound (e.g., `<= c`). *)

(**
 * @function compare_bound
 * @brief Custom comparison function for `bound` types.
 * Orders `Less c` before `LessEq c` if `c` values are equal, otherwise compares `c` values.
 * This is necessary for creating an ordered set of bounds (`BoundSet`).
 *
 * @param b1 The first bound.
 * @param b2 The second bound.
 * @return An integer compatible with `Set.Make` comparison (-1, 0, or 1).
 *)
let compare_bound b1 b2 =
  match (b1, b2) with
  | Less c1, Less c2 -> compare c1 c2
  | LessEq c1, LessEq c2 -> compare c1 c2
  | Less c1, LessEq c2 ->
      let cmp = compare c1 c2 in
      if cmp = 0 then -1 (* Less c is "smaller" than LessEq c for the same c *) else cmp
  | LessEq c1, Less c2 ->
      let cmp = compare c1 c2 in
      if cmp = 0 then 1 (* LessEq c is "larger" than Less c for the same c *) else cmp

(**
 * @function satisfies_bound
 * @brief Checks if a float `f` satisfies a given `bound`.
 *
 * @param f The float value to check.
 * @param bound The bound to check against.
 * @return `true` if `f` satisfies `bound`, `false` otherwise.
 *)
let satisfies_bound f bound =
  match bound with
  | Less c -> f < c
  | LessEq c -> f <= c

(* == Bound Set == *)
(** Module defining the ordering for `bound` types, required by `Set.Make`. *)
module BoundOrder = struct
  type t = bound (** Element type is `bound`. *)
  let compare = compare_bound (** Uses the custom `compare_bound` function. *)
end

(** Module defining a set of `bound`s, using `BoundOrder` for ordering. *)
module BoundSet = Set.Make (BoundOrder)

(* == Adapters for Lat == *)
(**
 * @module FloatSetContents
 * @brief Adapter module that makes `(float, FloatSet.t) set_or_top` conform to the `Bag.Lat` signature.
 * This allows `FloatSet.t set_or_top` to be used as the content of a Bag.
 *)
module FloatSetContents : Lat with type t = (float, FloatSet.t) set_or_top =
struct
  type t = (float, FloatSet.t) set_or_top

  (** Union operation for `set_or_top FloatSet.t`. If either is `Top`, the result is `Top`.
      Otherwise, it's the union of the finite `FloatSet`s. *)
  let union v1 v2 =
    match (v1, v2) with
    | Top, _ -> Top
    | _, Top -> Top
    | Finite s1, Finite s2 -> Finite (FloatSet.union s1 s2)

  (** Equality for `set_or_top FloatSet.t`. Both must be `Top` or both must be `Finite`
      with equal `FloatSet`s. *)
  let equal v1 v2 =
    match (v1, v2) with
    | Top, Top -> true
    | Finite s1, Finite s2 -> FloatSet.equal s1 s2
    | _, _ -> false
end

(**
 * @module BoundSetContents
 * @brief Adapter module that makes `(bound, BoundSet.t) set_or_top` conform to the `Bag.Lat` signature.
 * This allows `BoundSet.t set_or_top` to be used as the content of a Bag.
 *)
module BoundSetContents : Lat with type t = (bound, BoundSet.t) set_or_top =
struct
  type t = (bound, BoundSet.t) set_or_top

  (** Union operation for `set_or_top BoundSet.t`. If either is `Top`, the result is `Top`.
      Otherwise, it's the union of the finite `BoundSet`s. *)
  let union v1 v2 =
    match (v1, v2) with
    | Top, _ -> Top
    | _, Top -> Top
    | Finite s1, Finite s2 -> Finite (BoundSet.union s1 s2)

  (** Equality for `set_or_top BoundSet.t`. Both must be `Top` or both must be `Finite`
      with equal `BoundSet`s. *)
  let equal v1 v2 =
    match (v1, v2) with
    | Top, Top -> true
    | Finite s1, Finite s2 -> BoundSet.equal s1 s2
    | _, _ -> false
end

(* == Bag Instantiations == *)
(**
 * @module FloatBag
 * @brief A Bag specialized for managing `FloatSet.t set_or_top`.
 * Used for tracking sets of possible float constants.
 *)
module FloatBag = Make (FloatSetContents)

(**
 * @module OriginalBoundBag
 * @brief A Bag specialized for managing `BoundSet.t set_or_top`.
 * This is the base version before adding convenience functions like `add_all`.
 *)
module OriginalBoundBag = Make (BoundSetContents)

(**
 * @module BoundBag
 * @brief An extended version of `OriginalBoundBag` that includes an `add_all` utility function.
 * This is the primary module used for managing comparison bounds.
 *)
module BoundBag = struct
  include OriginalBoundBag (* Inherit all functionality from OriginalBoundBag (create, leq, eq, get, listen). *)

  (**
   * @function add_all
   * @brief Adds all floats from a `FloatSet.t` as `LessEq` bounds to a `BoundBag.bag`.
   *
   * This function takes a set of floats and a `BoundBag.bag`. For each float `f`
   * in the set, it adds the bound `LessEq f` to the bag.
   * If the bag's content is `Top`, this operation has no effect.
   * The update is performed using `leq` to ensure listeners are correctly triggered.
   *
   * @param float_s The set of floats to add as bounds.
   * @param b_bag The `BoundBag.bag` to which the bounds will be added.
   *)
  let add_all (float_s : FloatSet.t) (b_bag : bag) : unit =
    let current_content = get b_bag in (* Get the current state of the bound bag (Finite set or Top) *)
    match current_content with
    | Top -> () (* If the bag is already Top (unconstrained), adding specific bounds has no effect. *)
    | Finite current_bound_set ->
        (* Convert each float in float_s to a LessEq bound. *)
        let new_bounds_to_add =
          FloatSet.fold
            (fun f acc_set -> BoundSet.add (LessEq f) acc_set) (* Add LessEq f for each float f *)
            float_s BoundSet.empty (* Start with an empty set of new bounds *)
        in
        (* Combine the existing bounds with the new ones. *)
        let potentially_updated_bound_set =
          BoundSet.union current_bound_set new_bounds_to_add
        in
        (* Only proceed if the set of bounds actually changes. *)
        if not (BoundSet.equal current_bound_set potentially_updated_bound_set)
        then
          (* To ensure listeners on b_bag are triggered correctly, create a temporary
             bag with the new desired state and use `leq`. This makes `b_bag`
             at least as constrained as `temp_bag_with_new_state`. *)
          let temp_bag_with_new_state =
            create (Finite potentially_updated_bound_set) (* Create a bag with the new complete set of bounds. *)
          in
          leq temp_bag_with_new_state b_bag (* Propagate this new state to the original b_bag. *)
end

(**
 * @function fresh_bound_bag
 * @brief Creates a new, empty `BoundBag.bag`.
 * The bag is initialized with `Finite BoundSet.empty`, meaning it initially has no bounds.
 * @return A new `BoundBag.bag`.
 *)
let fresh_bound_bag () : BoundBag.bag = BoundBag.create (Finite BoundSet.empty)

(**
 * @function fresh_float_bag
 * @brief Creates a new, empty `FloatBag.bag`.
 * The bag is initialized with `Finite FloatSet.empty`, meaning it initially contains no float constants.
 * @return A new `FloatBag.bag`.
 *)
let fresh_float_bag () : FloatBag.bag = FloatBag.create (Finite FloatSet.empty)
