(* GRADE:  100% *)
let listn1 n =
  let rec listenTail n acc list=
    if n=acc then n::list
    else listenTail (n+1) acc (n::list) in listenTail 0 n [] ;;
        

let length1 l =
  let rec lengTail l aux=
    match l with 
    |[]->aux
    |_::l'-> lengTail(l')(1+aux) in lengTail l 0;;

