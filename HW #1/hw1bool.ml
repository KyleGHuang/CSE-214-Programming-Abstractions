type bool_expr =
  | Lit of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let rec eval a a_bool b b_bool expr =
  match expr with
  | Lit var -> if var = a then a_bool else b_bool
  | Not ex -> not(eval a a_bool b b_bool ex)
  | And(e1, e2) -> (eval a a_bool b b_bool e1) && (eval a a_bool b b_bool e2)
  | Or(e1, e2) ->(eval a a_bool b b_bool e1) || (eval a a_bool b b_bool e2);;

let truth_table a b expr =
  [(true,  true,  eval a true b true expr);
   (true,  false, eval a true b false expr);
   (false, true,  eval a false b true expr);
   (false, false, eval a false b false expr)];;