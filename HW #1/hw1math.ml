(*Question 2*)
type expr =
  | Const of int
  | Var of string
  | Plus of arg
  | Mult of arg
  | Minus of arg
  | Div of arg
and arg = {arg1 : expr; arg2 : expr};;

(*Question 3*)
let rec evaluate expr = 
  match expr with
  | Const x -> x
  | Plus {arg1 = a ; arg2 = b} -> evaluate a + evaluate b
  | Mult {arg1 = a ; arg2 = b} -> evaluate a * evaluate b
  | Minus {arg1 = a ; arg2 = b} -> evaluate a - evaluate b
  | Div {arg1 = a ; arg2 = b} -> evaluate a / evaluate b;;