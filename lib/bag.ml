(* Implementation for Bags (Union-Find + Generic Join Structure) *)

(* Signature for the join-structure contents stored within a Bag *)
module type BAG_CONTENTS = sig
  type t                (* The type of the contents *)
  val union : t -> t -> t (* The join operation *)
end

(* Functor to create a Bag module for specific contents *)
module Make (C : BAG_CONTENTS) = struct

  (* Expose the content type *) 
  type t = C.t

  type bag_contents =
    | Root of { mutable current_val : C.t } (* Store value C.t *)
    | Link of bag
  and bag = bag_contents ref

  (* Find with path compression - unchanged *) 
  let rec find (b : bag) : bag =
    match !b with
    | Root _ -> b
    | Link parent ->
        let root = find parent in
        b := Link root;
        root

  (* Create a new bag with an initial value *) 
  let create (initial_val : C.t) : bag =
    ref (Root { current_val = initial_val })

  (* Union two bags, joining their values at the root *) 
  let union (b1 : bag) (b2 : bag) : unit =
    let r1 = find b1
    and r2 = find b2 in
    if r1 != r2 then (
      match !r1, !r2 with
      | Root data1, Root data2 ->
          (* Join contents using C.union into r1 *) 
          data1.current_val <- C.union data1.current_val data2.current_val; 
          (* Link r2 under r1. Content of r2 becomes inaccessible via get. *) 
          r2 := Link r1
      | _ ->
          assert false (* Should not happen after find *)
    )

  (* Get the current value associated with a bag (finds root first) *) 
  let get (b : bag) : C.t = 
    let r = find b in
    match !r with
    | Root data -> data.current_val
    | Link _ -> assert false (* Should not happen after find *)

  (* Expose the specialized Set module as well - REMOVED *)
end 