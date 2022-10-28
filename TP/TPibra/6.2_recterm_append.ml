(* GRADE:  100% *)
let rev_append left right =
  let rec appendTail left acc= 
    match left with 
    |[]-> acc 
    |e::r->appendTail r (e::acc) in appendTail left right;;

let append left right =
  rev_append (List.rev(left)) right;;

