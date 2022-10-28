(* GRADE:  100% *)
let size1 a =
  let rec aux_size a =
    match a with 
    |Nil->0
    |Node(_,fg,fd)->1+aux_size fg+ aux_size fd in aux_size a;; 

let size' l n =
  let rec aux l n =
    match l with 
    |[]->n
    |a::r-> match a with 
      |Nil-> aux r n
      |Node(_,fg,fd)->aux (List.append ([fg;fd]) (r)) (n+1) in aux l n;;
                                          

        
let size a =
  size' [a] 0;;

