open Bag (* Ensure this is at the top *)

(** Represents a value that can either be a finite set of elements ['elt] 
    (represented by type ['set]) or an abstract [Top] element, signifying 
    an unconstrained or universal set in the context of a lattice. *)
type ('elt, 'set) set_or_top = 
  | Finite of 'set (* Represents a specific, finite set of elements. *) 
  | Top            (* Represents the top element of the lattice (e.g., all possible values). *)

(* == Float Set == *)
(** Module for creating sets of floats. Uses standard float comparison. *)
module FloatSet = Set.Make(struct type t = float let compare = compare end)

(* == Bound Type == *)
(** Defines different types of bounds on floating-point numbers. *)
type bound = 
  | Less of float   (** Represents an exclusive upper bound (e.g., x < c). *)
  | LessEq of float (** Represents an inclusive upper bound (e.g., x <= c). *)
  | Greater of float   (** Represents an exclusive lower bound (e.g., x > c). *)
  | GreaterEq of float (** Represents an inclusive lower bound (e.g., x >= c). *)

(** Comparison function for [bound] types. This is used to establish a total 
    order for bounds, which is necessary for storing them in a [Set].
    The comparison prioritizes the type of bound when float values are equal:
    - [Less c] is considered "smaller" than [LessEq c].
    - [Greater c] is considered "smaller" than [GreaterEq c] (as it's more restrictive from below).
    This ordering ensures that, for example, in a set, [Less 5.0] would come before [LessEq 5.0].
    Comparisons between different categories (e.g., Less vs Greater) are based on the float values.
*)
let compare_bound b1 b2 = 
  match b1, b2 with
  (* Cases for comparing bounds of the same type (Less with Less, LessEq with LessEq, etc.) *)
  | Less c1, Less c2 -> compare c1 c2
  | LessEq c1, LessEq c2 -> compare c1 c2
  | Greater c1, Greater c2 -> compare c1 c2
  | GreaterEq c1, GreaterEq c2 -> compare c1 c2

  (* Cases for comparing Less with LessEq *)
  | Less c1, LessEq c2 -> 
      let cmp = compare c1 c2 in
      if cmp = 0 then -1 (* Less c is smaller than LessEq c if values are equal *) else cmp
  | LessEq c1, Less c2 ->
      let cmp = compare c1 c2 in
      if cmp = 0 then 1 (* LessEq c is greater than Less c if values are equal *) else cmp

  (* Cases for comparing Greater with GreaterEq *)
  | Greater c1, GreaterEq c2 -> 
      let cmp = compare c1 c2 in
      if cmp = 0 then -1 (* Greater c is smaller than GreaterEq c if values are equal *) else cmp
  | GreaterEq c1, Greater c2 ->
      let cmp = compare c1 c2 in
      if cmp = 0 then 1 (* GreaterEq c is greater than Greater c if values are equal *) else cmp

  (* Cross-category comparisons: based directly on float values.
     The specific ordering here (e.g. Less vs Greater) might seem arbitrary
     but provides a consistent total order. The primary use is for Set uniqueness. *)
  | Less _, Greater _ -> -1 (* Arbitrarily, Less types come before Greater types *)
  | Less _, GreaterEq _ -> -1
  | LessEq _, Greater _ -> -1
  | LessEq _, GreaterEq _ -> -1
  
  | Greater _, Less _ -> 1 (* Vice-versa *)
  | Greater _, LessEq _ -> 1
  | GreaterEq _, Less _ -> 1
  | GreaterEq _, LessEq _ -> 1

(** Checks if a float [f] satisfies a given [bound]. *)
let satisfies_bound (f : float) (b : bound) : bool =
  match b with
  | Less c -> f < c       (** [f] must be strictly less than [c]. *)
  | LessEq c -> f <= c    (** [f] must be less than or equal to [c]. *)
  | Greater c -> f > c    (** [f] must be strictly greater than [c]. *)
  | GreaterEq c -> f >= c (** [f] must be greater than or equal to [c]. *)


(* == Bound Set == *)
(** Module defining the ordering for [bound] types, used by [BoundSet]. *)
module BoundOrder = struct
  type t = bound
  let compare = compare_bound
end
(** Module for creating sets of [bound]s, using the custom [compare_bound] for ordering. *)
module BoundSet = Set.Make(BoundOrder)

(* == Adapters for Lat == *)
(** Adapter for [Bag.Lat] to handle float sets ([FloatSet.t]) within a [set_or_top] structure.
    This module defines how to compute the union of two float sets (or Top) and
    how to check them for equality, enabling [FloatSet.t set_or_top] to be used as 
    the content of a [Bag.Make] functor. *)
module FloatSetContents : Lat with type t = (float, FloatSet.t) set_or_top = struct
  type t = (float, FloatSet.t) set_or_top
  let union v1 v2 = 
    match v1, v2 with
    | Top, _ -> Top (* Union with Top results in Top *)
    | _, Top -> Top (* Union with Top results in Top *)
    | Finite s1, Finite s2 -> Finite (FloatSet.union s1 s2) (* Union of finite sets *)

  let equal v1 v2 = 
    match v1, v2 with
    | Top, Top -> true (* Top is equal to Top *)
    | Finite s1, Finite s2 -> FloatSet.equal s1 s2 (* Equality for finite sets *)
    | _, _ -> false (* Otherwise (Finite vs Top), they are not equal *)
end

(** Adapter for [Bag.Lat] to handle bound sets ([BoundSet.t]) within a [set_or_top] structure.
    Similar to [FloatSetContents], this defines union and equality for 
    [BoundSet.t set_or_top], allowing it to be used as content in a [Bag.Make] functor. *)
module BoundSetContents : Lat with type t = (bound, BoundSet.t) set_or_top = struct
  type t = (bound, BoundSet.t) set_or_top
  let union v1 v2 = 
    match v1, v2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Finite s1, Finite s2 -> Finite (BoundSet.union s1 s2)

  let equal v1 v2 =
    match v1, v2 with
    | Top, Top -> true
    | Finite s1, Finite s2 -> BoundSet.equal s1 s2
    | _, _ -> false
end

(* == Bag Instantiations == *)
(** A bag specialized to hold float sets ([FloatSet.t set_or_top]). *)
module FloatBag = Make(FloatSetContents)

(** The original instantiation of a bag for bound sets. 
    Used as the basis for the extended [BoundBag] module. *)
module OriginalBoundBag = Make(BoundSetContents)

(** Extended [BoundBag] module. It includes all functionality from 
    [OriginalBoundBag] (which is [Bag.Make(BoundSetContents)]) and adds 
    utility functions like [add_all]. *)
module BoundBag = struct
  include OriginalBoundBag (* Inherit all types and functions from OriginalBoundBag *)

  (** Adds a set of floats ([float_s]) to a [BoundBag.bag] ([b_bag]) by converting 
      each float into a [LessEq] bound.
      - If [b_bag] currently holds [Top], this function does nothing, as [Top] cannot
        be refined further by adding finite bounds.
      - Otherwise, it computes the union of the existing bounds in [b_bag] and the 
        new [LessEq] bounds derived from [float_s].
      - If this union results in a change to [b_bag]'s content, the bag is updated.
        This update is done by creating a temporary bag with the new desired state 
        and using [leq temp_bag b_bag] to propagate the change, ensuring any 
        listeners on [b_bag] are correctly triggered. *)
  let add_all (float_s : FloatSet.t) (b_bag : bag) : unit =
    let current_content = get b_bag in
    match current_content with
    | Top -> () (* If the bag is already Top, no change needed or possible by adding finite bounds. *)
    | Finite current_bound_set ->
        (* Convert each float in float_s to a LessEq bound and collect them into a new BoundSet. *)
        let new_bounds_to_add = 
          FloatSet.fold (fun f acc_set -> BoundSet.add (LessEq f) acc_set) float_s BoundSet.empty 
        in
        (* Calculate the union of the current bounds and the new bounds. *)
        let potentially_updated_bound_set = BoundSet.union current_bound_set new_bounds_to_add in
        (* Only proceed with an update if the new set of bounds is actually different from the current one. *)
        if not (BoundSet.equal current_bound_set potentially_updated_bound_set) then
          (* Create a temporary bag holding the new desired state (Finite potentially_updated_bound_set).
             Then, use leq to enforce that b_bag's content becomes at least this new state.
             This ensures that listeners on b_bag are triggered appropriately due to the change. *)
          let temp_bag_with_new_state = create (Finite potentially_updated_bound_set) in
          leq temp_bag_with_new_state b_bag
end

(** Utility function to create a new, empty [BoundBag.bag].
    The bag is initialized with a [Finite] empty [BoundSet.t]. *)
let fresh_bound_bag () : BoundBag.bag =
  BoundBag.create (Finite BoundSet.empty)

(** Utility function to create a new, empty [FloatBag.bag].
    The bag is initialized with a [Finite] empty [FloatSet.t]. *)
let fresh_float_bag () : FloatBag.bag =
  FloatBag.create (Finite FloatSet.empty)