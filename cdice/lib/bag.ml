(**
 * @file bag.ml
 * @brief Implements the Bag data structure for managing lattice elements with propagation.
 *
 * A Bag is a container for a lattice value (`L.t`) that supports listeners.
 * When the value in a Bag changes (specifically, when it increases in the lattice order),
 * registered listeners are triggered. This mechanism is used to propagate constraints
 * between different parts of the type system (e.g., float bounds and constants).
 * The `Make` functor allows creating Bag modules for any type that satisfies the `Lat` signature.
 *)

(**
 * @module type Lat
 * @brief Signature for lattice elements that can be stored in a Bag.
 *
 * To be used with the `Bag.Make` functor, a type `t` must provide:
 * - A `union` operation (least upper bound or join in the lattice).
 * - An `equal` operation to check for equality.
 *)
module type Lat = sig
  type t (** The type of the lattice element. *)

  val union : t -> t -> t (** Computes the least upper bound (join) of two lattice elements. *)
  val equal : t -> t -> bool (** Checks if two lattice elements are equal. *)
end

(**
 * @module Make
 * @brief Functor to create a Bag module for a specific lattice type.
 * @param L A module satisfying the `Lat` signature.
 *)
module Make (L : Lat) = struct
  (** The type of the content managed by this Bag instance, derived from the lattice `L`. *)
  type t = L.t

  (** Internal record structure of a bag.
      - `content`: A mutable reference to the current lattice value.
      - `listeners`: A list of zero-argument functions to be called when `content` changes. *)
  type bag_record = {
    content : L.t ref;
    mutable listeners : (unit -> unit) list;
  }

  (** The abstract `bag` type. Users interact with this type, not `bag_record` directly. *)
  type bag = bag_record

  (**
   * @function atomic_update
   * @brief Updates the content of a bag and notifies listeners if the value has changed.
   *
   * This function is "atomic" in the sense that it checks for equality before
   * updating and triggering listeners, preventing redundant notifications or cycles
   * if the new value is identical to the current one.
   *
   * @param b The bag to update.
   * @param new_val The new lattice value for the bag.
   *)
  let atomic_update (b : bag) (new_val : L.t) : unit =
    let current_val = !(b.content) in
    if not (L.equal current_val new_val) then (
      b.content := new_val;
      (* Trigger all registered listeners. *)
      List.iter (fun f -> f ()) b.listeners
    )

  (**
   * @function create
   * @brief Creates a new bag with an initial lattice value.
   * The bag starts with no listeners.
   *
   * @param initial_val The initial lattice value for the bag.
   * @return A new bag.
   *)
  let create (initial_val : L.t) : bag =
    { content = ref initial_val; listeners = [] }

  (**
   * @function listen
   * @brief Registers a listener function to a bag.
   *
   * The listener function is called immediately upon registration with the bag's
   * current value (implicitly, as the listener is `unit -> unit` but is called
   * in a context where it can access the bag). It will also be called whenever
   * the bag's content is updated via `atomic_update`.
   *
   * @param b The bag to listen to.
   * @param listener A function (`unit -> unit`) to be executed on updates.
   *)
  let listen (b : bag) (listener : unit -> unit) : unit =
    b.listeners <- listener :: b.listeners;
    listener () (* Call listener immediately to process the current state. *)

  (**
   * @function leq
   * @brief Enforces a lattice less-than-or-equal constraint (b1 <= b2).
   *
   * This function ensures that the value in bag `b2` is always greater than or equal to
   * (in the lattice sense, via `L.union`) the value in bag `b1`. It achieves this by:
   * 1. Immediately updating `b2` with `L.union !(b1.content) !(b2.content)`.
   * 2. Registering a listener on `b1` such that any future change to `b1`
   *    triggers a re-evaluation and potential update of `b2`.
   *
   * @param b1 The bag that should be "less than or equal to" `b2`.
   * @param b2 The bag that should be "greater than or equal to" `b1`.
   *)
  let leq (b1 : bag) (b2 : bag) : unit =
    (* This listener function will be called when b1 changes (or initially). *)
    let update_b2_from_b1 () =
      let v1 = !(b1.content) in (* Current value of b1 *)
      let v2 = !(b2.content) in (* Current value of b2 *)
      let merged_v = L.union v1 v2 in (* Compute the least upper bound *)
      atomic_update b2 merged_v (* Update b2 if merged_v is different/greater *)
    in
    (* Register the listener on b1. This also calls update_b2_from_b1 once immediately. *)
    listen b1 update_b2_from_b1

  (**
   * @function eq
   * @brief Enforces a lattice equality constraint (b1 = b2).
   *
   * This is achieved by establishing `b1 <= b2` and `b2 <= b1` simultaneously.
   * Any change in one bag will propagate to the other to maintain equality
   * with respect to the lattice operation `L.union`.
   *
   * @param b1 One bag.
   * @param b2 The other bag.
   *)
  let eq (b1 : bag) (b2 : bag) : unit =
    leq b1 b2; (* Ensure b2 is always "at least" b1 *)
    leq b2 b1  (* Ensure b1 is always "at least" b2 *)

  (**
   * @function get
   * @brief Retrieves the current lattice value from a bag.
   *
   * @param b The bag.
   * @return The current lattice value stored in `b`.
   *)
  let get (b : bag) : L.t = !(b.content)
end
