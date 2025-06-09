let var_counter = ref 0
let fresh_var (prefix : string) : string =
  incr var_counter;
  prefix ^ string_of_int !var_counter

let gen_let (base_name_hint : string) (rhs_expr : Ast.expr) (body_fn : string -> Ast.expr) : Ast.expr =
  match rhs_expr with
  | Ast.ExprNode (Ast.Var existing_var_name) ->
      body_fn existing_var_name
  | _ ->
      let new_var_name = fresh_var base_name_hint in
      Ast.ExprNode (Ast.Let (new_var_name, rhs_expr, body_fn new_var_name))

let bit_length n =
  if n < 0 then invalid_arg "bit_length: only non-negative integers allowed"
  else if n = 0 then 1
  else
    let rec aux n acc =
      if n = 0 then acc
      else aux (n lsr 1) (acc + 1)
    in
    aux n 0

(* For getting the correct int size in the to_dice conversion *)
let curr_max_int_sz = ref 0