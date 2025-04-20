(* Implementation for Bags (Lattice Elements + Propagation) *)

(* Signature for the lattice element contents stored within a Bag *)
module type Lat = sig
  type t                (* The type of the contents *)
  val union : t -> t -> t (* The join operation (least upper bound) *)
  val equal : t -> t -> bool (* Equality test *)
end

(* Functor to create a Bag module for specific lattice elements *)
module Make (L : Lat) = struct

  (* Expose the content type *) 
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
      (* Trigger listeners *) 
      List.iter (fun f -> f ()) b.listeners
    )

  (* Create a new bag with an initial value and no listeners *) 
  let create (initial_val : L.t) : bag =
    { content = ref initial_val; listeners = [] }

  (* Enforce b1 <= b2. Updates b2 and adds b1 as a listener to propagate changes. *) 
  let rec leq (b1 : bag) (b2 : bag) : unit =
    let v1 = !(b1.content) in
    let v2 = !(b2.content) in
    let merged_v = L.union v1 v2 in
    (* Update b2's value if necessary (atomic_update handles check & notification) *) 
    atomic_update b2 merged_v;
    (* Add a listener to b1: whenever b1 changes, re-enforce b1 <= b2 *) 
    (* TODO: Consider preventing duplicate listeners for efficiency, maybe using a Set? *) 
    b1.listeners <- (fun () -> leq b1 b2) :: b1.listeners

  (* Enforce b1 = b2 by making them mutually leq *) 
  let eq (b1 : bag) (b2 : bag) : unit =
    leq b1 b2;
    leq b2 b1

  (* Get the current value associated with a bag *) 
  let get (b : bag) : L.t = 
    !(b.content)

end 