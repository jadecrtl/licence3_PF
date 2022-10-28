(* GRADE:  100% *)
let plateau_initial (taille : int) : plateau =
  Matrix.init taille (fun (i,j)-> (Matrix.init taille (fun (i,j)->None)))
;;

let sous_morpion_valide
    (plateau : plateau)
    (dernier_coup : coup option)
    ((i,j) : int * int)
  : bool = 
  match dernier_coup with
  | None -> if termine_dim_1 (Matrix.get plateau (i,j)) then false else true
  | Some c ->
      let dim1 = c.position_dim_1 in 
      if termine_dim_1 (Matrix.get plateau dim1) then 
        if (i,j)=dim1 then false 
        else not (termine_dim_1 (Matrix.get plateau (i,j)))
      else if (i,j)=dim1 then true
      else false
;;

let coup_legal
    (plateau : plateau)
    (dernier_coup : coup option)
    (coup : coup)
  : bool =
  if sous_morpion_valide plateau dernier_coup coup.position_dim_2 then
    let t = Matrix.get (Matrix.get plateau coup.position_dim_2) coup.position_dim_1 in
    match t with
    | None -> true
    | _ -> false
  else false

;;

let prepare_coup
    (plateau : plateau)
    (dernier_coup : coup option)
    (nb_joueurs : int)
    ((x,y) : int * int)
  : coup option =
  let taille= Matrix.size plateau in
  if x<0 ||y<0 || x>=taille*taille|| y>=taille*taille then None 
  else match dernier_coup with 
    |None-> Some({joueur=0; position_dim_2 =(x/taille,y/taille); position_dim_1=(x mod taille,y mod taille)})
    |Some c->let joueur= c.joueur in 
        if joueur<(nb_joueurs - 1) then 
          Some({joueur=joueur+1;position_dim_2 =(x/taille,y/taille); position_dim_1=(x mod taille,y mod taille)})
        else Some ({joueur=0;position_dim_2 =(x/taille,y/taille); position_dim_1=(x mod taille ,y mod taille)})
;;

