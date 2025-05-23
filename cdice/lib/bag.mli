(* Interface file for Bags (Lattice Elements + Propagation) *)

(** Defines the signature for lattice elements that can be stored in a Bag.
    A lattice consists of a set of elements and two binary operations: join (union)
    and meet (intersection, though not explicitly required here for bag propagation).
    It must also have a partial order (leq). *)
module type Lat = sig
  (** The type of the lattice element. *)
  type t
  (** The join operation (least upper bound) of the lattice. 
      Computes `a \/ b`. Must be associative, commutative, and idempotent. *)
  val union : t -> t -> t
  (** Equality test for lattice elements. Required to check if a bag's
      content has actually changed before notifying listeners. *)
  val equal : t -> t -> bool
end

(** Functor to create a Bag module for a specific type of lattice element.
    A Bag is a container for a lattice value that supports propagation of changes.
    When the value in one bag is updated, other bags that depend on it (via `leq`
    or `eq` relationships) are automatically updated. *)
module Make (L : Lat) : sig

  (** The type of the lattice element contents held by the bag, inherited from `L.t`. *)
  type t = L.t 

  (** Abstract type for a bag. The internal representation is hidden to ensure
      that interactions occur only through the defined module functions. *)
  type bag 

  (** Creates a new bag, initializing it with the provided lattice value `initial_val`. *)
  val create : L.t -> bag

  (** Establishes a less-than-or-equal-to relationship: `b1 <= b2`.
      This has two effects:
      1. Immediately, `b2`'s value is updated to `L.union !(b1.content) !(b2.content)`.
      2. Henceforth, any change to `b1`'s content will trigger an update to `b2`'s 
         content, maintaining the `b1 <= b2` invariant. This is achieved by `b2`
         effectively "listening" to changes in `b1`.
      The operation is idempotent in terms of establishing the listener. *)
  val leq : bag -> bag -> unit

  (** Establishes an equality relationship: `b1 = b2`.
      This is enforced by making `b1 <= b2` and `b2 <= b1`.
      Changes to either bag will propagate to the other to maintain equality. *)
  val eq : bag -> bag -> unit

  (** Returns the current lattice value stored in the bag. This is a direct
      retrieval of the current state of the bag's content. *)
  val get : bag -> L.t

  (** Registers a listener function `f` that is executed whenever the bag's 
      content changes. The listener `f` is also executed once immediately upon 
      registration with the bag's current value. 
      Listeners are called after the bag's content has been updated. *)
  val listen : bag -> (unit -> unit) -> unit

end 