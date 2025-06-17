open Lat (* Ensure this is at the top *)

(* Generic type representing either a finite Set or Top *) 
type ('elt, 'set) set_or_top = 
  | Finite of 'set (* Store the actual Set instance *) 
  | Top

(* == Float Set == *)
module FloatSet = Set.Make(struct type t = float let compare = compare end)

(* == Cut Type == *)
type cut = 
  | Less of float   (* < c *)
  | LessEq of float (* <= c *)

let compare_cut b1 b2 = 
  match b1, b2 with
  | Less c1, Less c2 -> compare c1 c2
  | LessEq c1, LessEq c2 -> compare c1 c2
  | Less c1, LessEq c2 -> 
      let cmp = compare c1 c2 in
      if cmp = 0 then -1 else cmp
  | LessEq c1, Less c2 ->
      let cmp = compare c1 c2 in
      if cmp = 0 then 1 else cmp

let satisfies_cut f cut =
  match cut with
  | Less c -> f < c
  | LessEq c -> f <= c


(* == Cut Set == *)
module CutOrder = struct
  type t = cut
  let compare = compare_cut
end
module CutSet = Set.Make(CutOrder)

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

module CutSetContents : Lat with type t = (cut, CutSet.t) set_or_top = struct
  type t = (cut, CutSet.t) set_or_top
  let union v1 v2 = 
    match v1, v2 with
    | Top, _ -> Top
    | _, Top -> Top
    | Finite s1, Finite s2 -> Finite (CutSet.union s1 s2)

  let equal v1 v2 =
    match v1, v2 with
    | Top, Top -> true
    | Finite s1, Finite s2 -> CutSet.equal s1 s2
    | _, _ -> false
end

(* == Lat Instantiations == *)
module FloatLat = Make(FloatSetContents)

(* Original CutLat module *)
module OriginalCutLat = Make(CutSetContents)

(* Extended CutLat module to include add_all *)
module CutLat = struct
  include OriginalCutLat (* Include all existing functionality *)

  (* Function to add all floats from a FloatSet as LessEq cuts to a CutLat *)
  let add_all (float_s : FloatSet.t) (b_bag : bag) : unit =
    let current_content = get b_bag in
    match current_content with
    | Top -> () (* If the bag is already Top, no change or cannot add *)
    | Finite current_cut_set ->
        (* Create a set of new cuts from the float set *)
        let new_cuts_to_add = 
          FloatSet.fold (fun f acc_set -> CutSet.add (LessEq f) acc_set) float_s CutSet.empty 
        in
        (* The potential new state of the cut set *)
        let potentially_updated_cut_set = CutSet.union current_cut_set new_cuts_to_add in
        (* Only update if there's an actual change *)
        if not (CutSet.equal current_cut_set potentially_updated_cut_set) then
          (* Use leq with a temporary bag to ensure listeners are triggered *)
          let temp_bag_with_new_state = create (Finite potentially_updated_cut_set) in
          leq temp_bag_with_new_state b_bag
end

let fresh_cut_bag () : CutLat.bag =
  CutLat.create (Finite CutSet.empty)

let fresh_float_bag () : FloatLat.bag =
  FloatLat.create (Finite FloatSet.empty)