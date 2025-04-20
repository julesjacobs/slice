(* Implementation for Bags (Union-Find + FloatSet) *)

module FloatSet = Set.Make(struct
  type t = float
  let compare = compare
end)

(* A bag is a ref to either:
   - Root { elems }  : the canonical node holding a set of floats
   - Link parent     : pointing up to another bag
*)
type bag_contents =
  | Root of { mutable elems : FloatSet.t }
  | Link of bag

and bag = bag_contents ref

let new_bag () : bag =
  ref (Root { elems = FloatSet.empty })

(* Find with path compression *)
let rec find (b : bag) : bag =
  match !b with
  | Root _ -> b
  | Link parent ->
      let root = find parent in
      b := Link root;
      root

(* Union two bags, merging their FloatSets at the new root *)
let assert_eq (b1 : bag) (b2 : bag) : unit =
  let r1 = find b1
  and r2 = find b2 in
  if r1 != r2 then (
    match !r1, !r2 with
    | Root data1, Root data2 ->
        (* merge into r1 *)
        data1.elems <- FloatSet.union data1.elems data2.elems;
        (* clear the old root and link it under r1 *)
        data2.elems <- FloatSet.empty;
        r2 := Link r1
    | _ ->
        assert false  (* impossible: after find both must be Root *)
  )

(* Record that float x ∈ bag *)
let assert_elem (x : float) (b : bag) : unit =
  let r = find b in
  match !r with
  | Root data -> data.elems <- FloatSet.add x data.elems
  | Link _ -> assert false  (* impossible after find *) 