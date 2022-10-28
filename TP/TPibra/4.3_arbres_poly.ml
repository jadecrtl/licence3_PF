(* GRADE:  100% *)
let rec forall_labels p a =
  match a with
  |Nil ->true
  |Node(n,g,d) when p n -> forall_labels p g && forall_labels p d
  |Node (n,g,d)->false;;

let is_uniform x a =
  forall_labels (fun e-> e=x) a;;

let rec forall_subtrees pn a =
  match a with
  |Nil->true
  |Node(n,g,d)-> if pn n g d then forall_subtrees pn g && forall_subtrees pn d
      else false;;

let is_right_comb a =
  match a with 
  |Nil->true
  |Node(x,g,d)->forall_subtrees (fun x g d-> 
      if g=Nil then true 
      else false) a;;

let sum a =
  fold_tree (fun x y z-> x+y+z) 0 a;;




let map_tree f a =
  let rec fold fn vf a =
    match a with
    | Nil -> vf
    | Node (n, g, d) -> Node (fn n, (fold fn vf g), (fold fn vf d))in 
  fold (fun e-> (f e)) Nil a;;

