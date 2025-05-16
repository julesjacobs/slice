open Bag (* Ensure this is at the top *)

(* Generic type representing either a finite Set or Top *) 
type ('elt, 'set) set_or_top = 
  | Finite of 'set (* Store the actual Set instance *) 
  | Top

(* == Float Set == *)
module FloatSet = Set.Make(struct type t = float let compare = compare end)

(* == Bound Type == *)
type bound = 
  | Less of float   (* < c *)
  | LessEq of float (* <= c *)

let compare_bound b1 b2 = 
  match b1, b2 with
  | Less c1, Less c2 -> compare c1 c2
  | LessEq c1, LessEq c2 -> compare c1 c2
  | Less c1, LessEq c2 -> 
      let cmp = compare c1 c2 in
      if cmp = 0 then -1 else cmp
  | LessEq c1, Less c2 ->
      let cmp = compare c1 c2 in
      if cmp = 0 then 1 else cmp

(* == Bound Set == *)
module BoundOrder = struct
  type t = bound
  let compare = compare_bound
end
module BoundSet = Set.Make(BoundOrder)

(* == Adapters for Lat == *)
module FloatSetContents : Lat with type t = (float, FloatSet.t) set_or_top = struct
  type t = (float, FloatSet.t) set_or_top
  let union v1 v2 = 
    match v1, v2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Finite s1, Finite s2 -> Finite (FloatSet.union s1 s2)

  let equal v1 v2 = 
    match v1, v2 with
    | Top, Top -> true
    | Finite s1, Finite s2 -> FloatSet.equal s1 s2
    | _, _ -> false
end

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
module FloatBag = Make(FloatSetContents)

(* Original BoundBag module *)
module OriginalBoundBag = Make(BoundSetContents)

(* Extended BoundBag module to include add_all *)
module BoundBag = struct
  include OriginalBoundBag (* Include all existing functionality *)

  (* Function to add all floats from a FloatSet as LessEq bounds to a BoundBag *)
  let add_all (float_s : FloatSet.t) (b_bag : bag) : unit =
    let current_content = get b_bag in
    match current_content with
    | Top -> () (* If the bag is already Top, no change or cannot add *)
    | Finite current_bound_set ->
        (* Create a set of new bounds from the float set *)
        let new_bounds_to_add = 
          FloatSet.fold (fun f acc_set -> BoundSet.add (LessEq f) acc_set) float_s BoundSet.empty 
        in
        (* The potential new state of the bound set *)
        let potentially_updated_bound_set = BoundSet.union current_bound_set new_bounds_to_add in
        (* Only update if there's an actual change *)
        if not (BoundSet.equal current_bound_set potentially_updated_bound_set) then
          (* Use leq with a temporary bag to ensure listeners are triggered *)
          let temp_bag_with_new_state = create (Finite potentially_updated_bound_set) in
          leq temp_bag_with_new_state b_bag
end

let fresh_bound_bag () : BoundBag.bag =
  BoundBag.create (Finite BoundSet.empty)

let fresh_float_bag () : FloatBag.bag =
  FloatBag.create (Finite FloatSet.empty)