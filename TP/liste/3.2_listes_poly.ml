(* Toutes les fonctions sont ici définies par des "let rec". Il est
   possible que dans vos solutions certaines fonctions ne soient pas
   récursives, car elles utilisent une fonction récursive auxiliaire
   définie localement. À vous d'enlever le mot clef "rec" dans ce
   cas. *)

let rec map f l = 
  match l with
  |[] -> []
  |x :: l -> f(x) :: map f l;;
    
let rec filter p l =
  match l with
  |[] -> []
  |x :: l -> if p(x) == true then x :: filter p l else filter p l;;

let rec append l1 l2 =
  match l1 with
  |[] -> l2
  |x1 :: r1 -> x1 :: (append r1 l2);;

let rec rev l =
  let rec aux x y =
    match x with
    |[] -> y
    |s :: z -> aux z (s :: y) 
  in aux l [];;

let rec flatten l =
  match l with 
  |[] -> []
  |x :: l -> append x (flatten l);;

let rec rotation_d l =
  let rec last l =
    match l with
    | [] -> []
    | [x] -> [x]
    | _ :: r -> last r in
  let rec remove_last l =
    match l with
    | [] -> []
    | [x] -> []
    | x :: r -> x :: remove_last r in
  let last = last l in append last (remove_last l);;