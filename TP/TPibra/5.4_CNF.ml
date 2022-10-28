(* GRADE:  100% *)
let rec desc_neg f =
  match f with 
  |Prop p-> Prop p 
  |Neg(Prop p)->Neg(Prop p) 
  |Neg(Neg phi)-> desc_neg phi 
  |And(phi1,phi2)->And(desc_neg phi1,desc_neg phi2)
  |Or(phi1,phi2)-> Or(desc_neg phi1, desc_neg phi2) 
  |Neg(And(phi1,phi2))->Or(desc_neg (Neg phi1),desc_neg(Neg phi2))
  |Neg(Or(phi1,phi2))-> And(desc_neg(Neg phi1),desc_neg (Neg  phi2)) ;;

let rec desc_or f =
  match f with 
  |Prop p->Prop p
  |Neg phi->Neg (desc_or(phi)) 
  |And(phi1,phi2)->And(desc_or phi1,desc_or phi2) 
  |Or(phi1,phi2)-> (let psi1= desc_or(phi1) in let psi2=desc_or (phi2) in 
                    match (psi1,psi2) with 
                    |(And(p,q),r)-> desc_or(And (Or(p,r),(Or(q,r))))
                    |(r,And(p,q))-> desc_or(And (Or(r,p),(Or(r,q))))
                    |(_,_)->Or(psi1,psi2));;
     

let cnf f =
  desc_or(desc_neg f);;  

