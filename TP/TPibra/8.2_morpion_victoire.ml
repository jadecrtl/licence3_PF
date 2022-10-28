(* GRADE:  100% *)
let ligne (m : 'a Matrix.t) (i : int) : 'a list =
  List.init (Matrix.size m)(fun y-> Matrix.get m(i,y))
;;

let colonne (m : 'a Matrix.t) (j : int) : 'a list =
  List.init (Matrix.size m)(fun x-> Matrix.get m(x,j))
;;

let diagonale (m : 'a Matrix.t) (k : int) : 'a list =
  if k=0 then List.init  (Matrix.size m)(fun x-> Matrix.get m(x,x))
  else let taille = Matrix.size m in 
    List.init taille (fun x-> Matrix.get m(x,taille-x-1))
;;

let gagnant_liste (l : 'a option list) : 'a option =
  let rec aux l = 
    match l with 
    |[]->None
    |None::r->None
    |Some x::r ->match r with 
      |[]->Some x
      |Some y::_-> if (x=y) then aux r
          else None 
      |_->None in aux l
    
;;

let premier_succes (n : int)(f : int -> 'a option) : 'a option =
  let rec aux n cpt f= 
    if cpt=n then None 
    else match f cpt with 
      |None-> aux n (cpt+1) f
      |Some x->Some x
  in aux n 0 f
;;

let gagnant_lignes (m : 'a option Matrix.t) : 'a option =
 
  premier_succes (Matrix.size m) (fun x-> gagnant_liste (ligne m x))
  
;;

let gagnant_colonnes (m : 'a option Matrix.t) : 'a option =
  premier_succes (Matrix.size m) (fun x-> gagnant_liste (colonne m x))
;;

let gagnant_diagonales (m : 'a option Matrix.t) : 'a option =
  premier_succes (2) (fun x-> gagnant_liste (diagonale m x))
;;

let gagnant (m : 'a option Matrix.t) : 'a option =
  match gagnant_lignes m with 
  |Some x-> Some x
  |None -> match gagnant_colonnes m with 
    |Some x->Some x
    |None -> match gagnant_diagonales m with
      |Some x-> Some x
      |None -> None
;;

let rec rempli_liste liste= match liste with 
  |[]->true
  |None::r->false
  |_::r->rempli_liste r;;
  
let rempli (n : int)(f : int -> bool) : bool =
  let rec aux n cpt f= 
    if cpt=n then true
    else match f cpt with 
      |false->false
      |true-> aux n (cpt+1) f 
  in aux n 0 f
;;

let termine (m : 'a option Matrix.t) : bool =
  if gagnant m = None then let taille= Matrix.size m in 
    rempli taille (fun x-> rempli_liste (ligne m x)) || 
    rempli taille (fun x-> rempli_liste (colonne m x))||
    rempli 2 (fun x-> rempli_liste (diagonale m x))
  else true

