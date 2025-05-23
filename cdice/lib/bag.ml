(* Implementation for Bags (Lattice Elements + Propagation) *)

(* Signature for the lattice element contents stored within a Bag *)
module type Lat = sig
  type t                (* The type of the contents *)
  val union : t -> t -> t (* The join operation (least upper bound) *)
  val equal : t -> t -> bool (* Equality test *)
end

(* Functor to create a Bag module for specific lattice elements *)
module Make (L : Lat) = struct

  type t = L.t

  (** Internal record structure for a bag. It holds a reference to the 
      lattice [content] and a list of [listeners] to be notified on 
      content changes. *)
  type bag_record = { 
    content   : L.t ref;
    mutable listeners : (unit -> unit) list;
  }
  
  type bag = bag_record

  (** Atomically updates the content of bag [b] to [new_val]. 
      If [new_val] is different from the current content (checked using 
      [L.equal]), the content is updated, and all registered listeners 
      for bag [b] are triggered. *)
  let atomic_update (b : bag) (new_val : L.t) : unit =
    let current_val = !(b.content) in
    if not (L.equal current_val new_val) then (
      b.content := new_val;
      List.iter (fun f -> f ()) b.listeners
    )

  (** Creates a new bag, initializing its content with [initial_val] and 
      an empty list of listeners. *)
  let create (initial_val : L.t) : bag =
    { content = ref initial_val; listeners = [] }

  (** Adds the function [listener] to the bag [b]'s list of listeners. 
      The [listener] is also executed once immediately after being added,
      using the bag's current content. *)
  let listen (b : bag) (listener : unit -> unit) : unit =
    b.listeners <- listener :: b.listeners;
    listener () (* Call listener immediately after registration *)

  (** Enforces the lattice relationship [b1 <= b2].
      This is achieved by making [b2] listen to [b1]. The listener function, 
      [update_b2_from_b1], when triggered by a change in [b1]'s content or 
      upon its initial registration (due to the behavior of [listen]), updates 
      [b2]'s content to the union of [b1]'s current content and [b2]'s 
      current content. [atomic_update] is used to make this change to [b2],
      which in turn will notify any listeners of [b2] if its value changes. *)
  let leq (b1 : bag) (b2 : bag) : unit =
    let update_b2_from_b1 () =
      let v1 = !(b1.content) in
      let v2 = !(b2.content) in 
      let merged_v = L.union v1 v2 in
      atomic_update b2 merged_v
    in
    listen b1 update_b2_from_b1

  (** Enforces equality [b1 = b2] by establishing [leq] relationships 
      in both directions: [b1 <= b2] and [b2 <= b1]. This ensures that 
      any change in one bag propagates to the other, and they converge 
      to the same value (their join). *)
  let eq (b1 : bag) (b2 : bag) : unit =
    leq b1 b2;
    leq b2 b1

  (** Retrieves the current lattice value from the bag [b] by dereferencing 
      its [content] field. *)
  let get (b : bag) : L.t = 
    !(b.content)

end 