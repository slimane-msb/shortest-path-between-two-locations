
(****************************************************************************************
                                        Version 2   
****************************************************************************************)
open Version1;;
(****************************************************************************************
                                        Partie A   
****************************************************************************************)

(**
   Type de données pour représenter un quadtree dont les régions libres
   sont numérotées. Pendant la construction, affectez arbitrairement le
   numéro (-1) à toutes les régions. La fonction  (numerote)  fournie 
   donnera un numéro unique à chaque région une fois l'arbre complet. 
*)
type qtree =
  | Libre of int (* numéro *)
  | Mur
  | Quad of qtree * qtree * qtree * qtree (* no, ne, so, se *)

(**
   Fonction de numérotation des quadtrees
     numerote: qtree -> int -> qtree * int

   L'appel  (numerote qt k)  renvoie une paire  (qt', k')  où 
   - (qt') est un quadtree de même structure que (qt) mais dont les 
     régions libres sont numérotées consécutivement à partir de (k)
   - (k') est l'entier qui suit le dernier numéro utilisé
 *)
let rec numerote qt k = match qt with
  | Libre _ -> Libre k, k+1
  | Mur     -> Mur, k
  | Quad(no, ne, so, se) ->
     let no, k = numerote no k in
     let ne, k = numerote ne k in
     let so, k = numerote so k in
     let se, k = numerote se k in
     Quad(no, ne, so, se), k

(**
   Affichage d'un quadtree 
   (vue hiérarchique avec retrait proportionnel à la profondeur)
 *)     
open Printf
let print_qtree qt =
  let offset n = String.make n ' ' in
  let rec print o = function
    | Mur -> printf "%sMur\n" (offset o)
    | Libre k -> printf "%sLibre %d\n" (offset o) k
    | Quad(no, ne, so, se) ->
       printf "%sQuad\n" (offset o);
       print (o+2) no;
       print (o+2) ne;
       print (o+2) so;
       print (o+2) se
  in
  print 0 qt

(*
     appartient_rect x y dx dy xCoord yCoord : int->int->int->int->int->int->bool
     :param: 
      x y dx dy representent le rectangle 
      xCoord yCoord representent les coordonnees d'une case dans la grille 
      :return: true si la case est dans le rectangle 
*)
let appartient_rect x y dx dy xCoord yCoord =
  (xCoord<(x+dx) && xCoord>=x && yCoord<(y+dy) && yCoord>=y)


(**
  signature: mur2atree_basique: int -> int -> int -> int -> int -> int -> int -> qtree
    :param: 
      x y dx dy : pour le rectangle 
      xCoord yCoord : les corrdonees du carree de taille n en rapport au carree initiale, 
    :return: qtree non simplifie d'un terrain 

*)
let rec mur2qtree_basique x y dx dy n xCoord yCoord = 
  if n=1 then 
      if (appartient_rect x y dx dy xCoord yCoord) then Mur
      else Libre
  else 
    Quad( mur2qtree_basique x y dx dy n/2 (xCoord)       (yCoord + n/2) ,
          mur2qtree_basique x y dx dy n/2 (xCoord + n/2) (y0 + n/2) ,
          mur2qtree_basique x y dx dy n/2 (x0)           (y0) ,
          mur2qtree_basique x y dx dy n/2 (xCoord + n/2) (y0) )

(*
    mur2qtree: int -> int -> int -> int -> int -> qtree telle que mur2qtree x y dx dy  n
    renvoie un quadtree représentant un terrain de côté n, qui est libre à l’exclusion d’un rectangle décrit par les
    coordonnées (x,y) de son coin sud-ouest, sa largeur dx et sa hauteur dy
    à chaque feuille Libre on associe un nombre entier, qui permettra de l’identifer
    en tant que nœud du graphe < num =-1 > par defautl 

    :signature: mur2qtree: int -> int -> int -> int -> int -> qtree
    :param: 
      x : int : 

    1. construire l'arbre representant chaque case en fonction du rectangle 
    2. appeler la fonction simplifier arabre 
*)
let mur2qtree x y dx dy n =
  simplifie (mur2qtree_basique x y dx dy n 0 0)






(**
   Fonction de construction d'un quadtree à partir d'une liste de
   régions intraversables
     list2qtree: int -> (int * int * int * int) list -> qtree

   Arguments : 
   - (n) est la longueur du côté du terrain
   - (l) est la liste des rectangles intraversables
 *)
let rec list2qtree n l =
  failwith "not implemented"

(**
   Fonction de calcul des coordonnées des centres des régions libres.
   Renvoie un tableau de paires de coordonnées.
   
   Arguments :
   - (qt) le quadtree
   - (k) le nombre de régions libres
   - (n) la longueur du côté du terrain
   Pré-condition :
   - les régions doivent être numérotées de 0 à k-1
 *)
let mk_coords qt k n =
  failwith "not implemented"
  




