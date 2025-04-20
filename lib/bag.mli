(* Interface file for Bags (Lattice Elements + Propagation) *)

(* Signature for the lattice element contents stored within a Bag *)
module type Lat = sig
  type t                (* The type of the contents *)
  val union : t -> t -> t (* The join operation (least upper bound) *)
  val equal : t -> t -> bool (* Equality test *)
end

(* Functor to create a Bag module for specific lattice elements *)
module Make (L : Lat) : sig

  (* The type of the contents held by the bag *)
  type t = L.t 

  (* Abstract type for a bag. Implementation details are hidden. *)
  type bag 

  (* Create a new bag containing an initial value *)
  val create : L.t -> bag

  (* Enforce that bag b1 is less than or equal to bag b2 in the lattice. 
     This updates b2 with the union of b1 and b2, and sets up propagation
     so future changes to b1 also update b2. *)
  val leq : bag -> bag -> unit

  (* Enforce that bag b1 and bag b2 are equal in the lattice. 
     Equivalent to leq b1 b2 and leq b2 b1. *)
  val eq : bag -> bag -> unit

  (* Get the current lattice value associated with a bag. *)
  val get : bag -> L.t

end 