(*later updates################################################################################################ *)

(*
  get_all_arrete_non_optimise : quad -> arrete list
  :return: toutes les arretes entres les zones libres
    <version non pas encore optimisee>
    - avoir les arretes horizontales et verticales 
    - appel recursive pour avoir les arretes internes 
*)
(* let get_all_arrete qt = 
  match qt with 
  | Libre(s) | Mur -> [] 
  | Quad(qt1,qt2,qt3,qt4) -> 
    (interface_verticale qt1 qt3)@(interface_verticale qt2 qt4)
    @(interface_horizontale qt1 qt2)@(interface_horizontale qt3 qt4)
     *)
(*################################################################################################ 
optimisation : 
- fusioner hori et verti 
- recherche voisin optimise it so that it's not going to look from all the list 
    - sort then get all neibors ( actually this is the graph)


ideas: 
    - make interface hori makes only outisde arretes, then call recursively for inside arrete



*)

