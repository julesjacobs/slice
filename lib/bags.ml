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

(* == Adapters for BAG_CONTENTS == *)
module FloatSetContents : BAG_CONTENTS with type t = (float, FloatSet.t) set_or_top = struct
  type t = (float, FloatSet.t) set_or_top
  let union v1 v2 = 
    match v1, v2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Finite s1, Finite s2 -> Finite (FloatSet.union s1 s2)
end

module BoundSetContents : BAG_CONTENTS with type t = (bound, BoundSet.t) set_or_top = struct
  type t = (bound, BoundSet.t) set_or_top
  let union v1 v2 = 
    match v1, v2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Finite s1, Finite s2 -> Finite (BoundSet.union s1 s2)
end

(* == Bag Instantiations == *)
module FloatBag = Make(FloatSetContents)
module BoundBag = Make(BoundSetContents)