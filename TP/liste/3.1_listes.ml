(* Toutes les fonction sont ici définies par des "let rec". Il est
   possible que dans vos solutions certaines fonctions ne soient pas
   récursives, car elles utilisent une fonction recursive auxiliaire
   définie localement. À vous d'enlever le mot clef "rec" dans ce
   cas. *)

let rec list_length l =
  match l with
  | [] -> 0 
  | _ :: l -> 1 + list_length l;;

let rec list_product l =
  match l with
  | [] -> 1
  | x :: l -> x * list_product l;; 
    
let rec mem x l =
  match l with
  | [] -> false
  | y :: l -> if y == x then true else mem x l;;

let min x y = if x < y then x else y;;

let rec list_min l =
  match l with 
  |[] -> failwith "Liste vide!"
  |[x] -> x
  |t :: l -> min t (list_min l);;

let rec last l =
  match l with
  |[] -> failwith "Liste vide!"
  |[x] -> x
  |t :: l -> last l;; 

let rec is_sorted l =
  match l with 
  |[] -> true
  |[x] -> true
  |t :: y :: l -> if t <= y then is_sorted(y::l) else false;;

let rec average l = 
  let rec somme l =
    match l with
    |[] -> failwith "Liste vide!"
    |[x] -> x
    |x :: l -> x + somme l
  in somme l/ list_length l;;

let rec nth l k =
  match l with
  |[] -> failwith "Liste vide!"
  |x :: _ when k = 0 -> x
  |_ :: l -> nth l (k-1);;

let rec range n m =
  if n = m then n :: [] else
  if n > m then n :: range (n - 1) m else
    n :: range (n + 1) m