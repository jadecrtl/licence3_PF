(* GRADE:  100% *)
let is_consequence f g =
  tautology (Or(Neg f,g));;

let are_equivalent f g =
  tautology (And(Or(Neg f,g),Or(Neg g, f)));;

