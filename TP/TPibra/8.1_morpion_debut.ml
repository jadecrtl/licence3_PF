(* GRADE:  100% *)
let plateau_initial (taille : int) : plateau =
  Matrix.init taille (fun (i,j)->None) 
;;


let coup_legal (plateau : plateau) (coup : coup) : bool =
  let position= coup.position in 
  if (Matrix.get plateau position= None) then true
  else false  
;;

let prepare_coup
    (plateau : plateau)
    (dernier_coup : coup option)
    (nb_joueurs : int)
    (x , y : int*int)
  : coup option =
  
  let taille=Matrix.size plateau in 
  if x>=taille ||x<0 || y<0 || y>=taille then None
  else match dernier_coup with 
    |None->Some({joueur=0;position=(x,y)})
    |Some dernier-> let joueur= dernier.joueur in 
        if joueur< (nb_joueurs - 1) then Some({joueur=joueur+1;position=(x,y)})
        else Some ({joueur=0;position=(x,y)})
;;

