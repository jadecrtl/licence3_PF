let is_consequence f g =
  let vf = list_of_props f 
  and vg = list_of_props g in 
  let v = union_sorted vf vg in 
  let affs = interpretations_props v in
  List.for_all (fun e  -> not (eval_formula f e)  || (eval_formula g e) ) affs

let are_equivalent f g =
  if (is_consequence f g && is_consequence g f) then true else false;;