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

  (* Internal representation of a bag *)
  type bag_record = { 
    content   : L.t ref;          (* Reference to the current lattice value *) 
    mutable listeners : (unit -> unit) list (* Functions to call on update *) 
  }
  
  (* Abstract bag type exposed in the interface *)
  type bag = bag_record

  (* Atomically update a bag's value and notify listeners if it changed. *)
  let atomic_update (b : bag) (new_val : L.t) : unit =
    let current_val = !(b.content) in
    if not (L.equal current_val new_val) then (
      b.content := new_val;
      List.iter (fun f -> f ()) b.listeners
    )

  (* Create a new bag with an initial value and no listeners *) 
  let create (initial_val : L.t) : bag =
    { content = ref initial_val; listeners = [] }

  (* Register an external listener and call it once immediately *) 
  let listen (b : bag) (listener : unit -> unit) : unit =
    b.listeners <- listener :: b.listeners;
    listener () (* Call listener immediately after registration *)

  (* Enforce b1 <= b2 using the listen mechanism. *)
  let leq (b1 : bag) (b2 : bag) : unit =
    (* Define the listener that updates b2 based on b1's value *)
    let update_b2_from_b1 () =
      let v1 = !(b1.content) in
      let v2 = !(b2.content) in 
      let merged_v = L.union v1 v2 in
      atomic_update b2 merged_v
    in
    (* Register the listener on b1. It will be called immediately once,
       and then again whenever b1's content changes. *)
    listen b1 update_b2_from_b1

  (* Enforce b1 = b2 by making them mutually leq *)
  let eq (b1 : bag) (b2 : bag) : unit =
    leq b1 b2;
    leq b2 b1

  (* Get the current value associated with a bag *) 
  let get (b : bag) : L.t = 
    !(b.content)

end 