let rec size a =
  match a with
  | Nil -> 0
  | Node (x,g,d) -> 1 + size g + size d;;

let rec depth a =
  match a with
  | Nil -> 0
  | Node (x,g,d) -> 1 + max (depth g) (depth d);;

let rec sum a =
  match a with
  | Nil -> 0
  | Node (x,g,d) -> x + sum g + sum d ;;

let rec contains x a =
  match a with
  | Nil -> false
  | Node (y,g,d) -> if y == x then true else (contains x g) or (contains x d);; 
  

let rec elements a =
  let rec aux a res =
    match a with
    |Nil -> res
    |Node(v,g,d) -> aux g (v :: (aux d res))
  in aux a [];;

let rec perfect a =
  match a with
  | Nil -> true
  | Node(v, g, d) -> ((depth g) == depth (d)) && perfect (g) && perfect (d);;
