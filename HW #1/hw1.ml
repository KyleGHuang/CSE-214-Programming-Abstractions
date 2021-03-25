(*Question 1*)
let rec pow x n =
  if n = 0 then 1
  else x * pow x (n - 1);;

let rec float_pow x n =
  if n = 0 then 1.0
  else x *. float_pow x (n - 1);;

(*Question 2*)
let lst_hd lst = 
  match lst with
  | [] -> []
  | [x] -> [x]
  | x::xs -> [x];;

let rec compress lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | x::xs ->
    if [x] = lst_hd xs then compress xs
    else x::compress xs;;

(*Question 3*)
let rec remove_if lst f =
  match lst with
  | [] -> []
  | [x] ->
    if (f x) then [] else [x]
  | x::xs ->
    if (f x) then remove_if xs f
    else x::remove_if xs f;;

(*Question 4*)
let rec lst_leng lst =
  match lst with
  | [] -> 0
  | [x] -> 1
  | x::xs -> 1 + lst_leng xs;;

let rec slice lst i j =
  match lst with
  | [] -> []
  | [x] ->
    if i > j then []
    else if i > 0 then []
    else [x]
  | x::xs ->
    if i > j then []
    else if i > lst_leng lst then []
    else if i > 0 then slice xs (i - 1) (j - 1)
    else if j > 1 then x::slice xs (i - 1) (j - 1)
    else [x];;

(*Question 5*)
let rec equivsCheck f n rest =
  match rest with
    [] -> []
  | x::xs ->
    if f n x then x::equivsCheck f n xs
    else equivsCheck f n xs;;

let rec equivsList f n lst =
  match lst with 
    [] -> []
  | x::xs ->
    if f n x then equivsList f n xs
    else x::equivsList f n xs;;

let rec equivs f lst =
  match lst with
  | [] -> []
  | x::xs ->
    equivsCheck f x lst :: equivs f (equivsList f x lst);;

(*Question 6*)
let rec prime x n =
  match x with
  | 1 -> false
  | _ -> match n with
      1 -> true
    | _ -> match x mod n with
        0 -> false
      | _ -> prime x (n - 1);;

let goldbachpair x =
  if (x mod 2 = 1) then failwith "Number not even."
  else if x <= 2 then failwith "Number not greater than 2."
  else let rec primeCheck a =
         if prime a (a - 1) && prime (x - a) (x - a - 1) then (x - a, a)
         else primeCheck (a - 1)
    in primeCheck x;;

(*Question 7*)
let rec equiv_on f g lst =
  match lst with
  |[] -> true
  |[x] ->
    if (f x) = (g x) then true
    else false
  | x::xs ->
    if (f x) = (g x) then equiv_on f g xs
    else false;;

(*Question 8*)
let rec pairwisefilter cmp lst =
  match lst with
  | [] -> []
  | [x] -> [x]
  | h::t::xs -> (cmp h t)::pairwisefilter cmp xs;;

(*Question 9*)
let rec pow x n =
  if n = 0 then 1
  else x * pow x (n - 1);;

let rec polynomial lst = fun x ->
  match lst with
  | [] -> 0
  | (a, b)::xs ->
    let there = polynomial xs in
    a * (pow x b) + there x;;

(*Question 10*)
let rec lst_map f lst =
  match lst with
  | [] -> []
  | x::xs -> (f x)::(lst_map f xs);;

let rec powerset lst =
  match lst with
  | [] -> [[]]
  | x::xs ->
    let aux = powerset xs in
    aux @ lst_map(fun n -> x::n) aux;;