
(****************************************************************************************
                                        Version 2   
****************************************************************************************)
open Version1;;
open Printf;;
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
   simplifie: qtree -> qtree telle que simplifie at renvoie un nouveau quadtree simplifiant
at si possible : si un nœud interne ne contient que des sous-arbres intégralement blancs ou intégralement noirs,
ce nœud interne doit être remplacé par une feuille

  :param: qt : quad tree
  :return: qt: quad tree simplifie 
  on renvoi le nombre associe a la premiere zone : Libre (1),Libre (2),Libre (3),Libre (4)) -> Libre (1)
*)
let rec simplifie qt = 
    match qt with 
      | Libre (nb) -> Libre (nb)
      | Mur -> Mur
      | Quad(qt1,qt2,qt3,qt4) -> 
          let qts = Quad(simplifie qt1,simplifie qt2,simplifie qt3,simplifie qt4) in 
          match qts with 
            | Quad(Mur,Mur,Mur,Mur) -> Mur
            | Quad(Libre (nb),Libre (nb2),Libre (nb3),Libre (nb4)) -> Libre (nb)
            |  _ -> qts 


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
  if n=1 then begin 
      if (appartient_rect x y dx dy xCoord yCoord) then Mur
      else Libre (-1)
    end 
  else 
    Quad( mur2qtree_basique x y dx dy (n/2) (xCoord)       (yCoord + n/2) ,
          mur2qtree_basique x y dx dy (n/2) (xCoord + n/2) (yCoord + n/2) ,
          mur2qtree_basique x y dx dy (n/2) (xCoord)           (yCoord) ,
          mur2qtree_basique x y dx dy (n/2) (xCoord + n/2) (yCoord) )

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


(*
  Une fonction d’intersection qui, étant donnés deux quadtrees qt1 et qt2, calcule un nouveau quadtree faisant
  l’intersection des régions libres de qt1 et qt2.  
  :signature: quad -> quad -> quad 
  :param: 
    qt1, qt2 : quadtree avec region libre(nb)/Mur
  :return: intersection des deux quadtree
*)
let rec inter qt1 qt2 = 
  match (qt1,qt2) with 
  | ( _ , Libre(a))  -> qt1
  | (Libre(a), _  ) -> qt2
  | ( _ , Mur) | (Mur, _ ) -> Mur
  | (Quad(qt11,qt12,qt13,qt14),Quad(qt21,qt22,qt23,qt24) ) -> Quad(inter qt11 qt21,inter qt12 qt22,inter qt13 qt23,inter qt14 qt24)


(*
  Une fonction qui combine les deux précédentes pour faire l’intersection des quadtrees correspondant à
  chaque région intraversable   
  :signature: int -> (int*int*int*int) list -> Quad
  :param: 
    - n taille du terrain 
    - listTerrain : (int*int*int*int) list pour : < x y dx dy > list
  :return: qtree representes par cette liste de rectangles 
    - avoir une list de qtree pour chaque rectangle
    - avoir l'intersection de ces qtree
*)
let rec list2qtree n listTerrain = 
  match listTerrain with
  | [] -> failwith "listTerrain vide" 
  | (x, y, dx, dy)::[] -> (mur2qtree x y dx dy n)
  | (x, y, dx, dy)::llistTerrain -> ( inter (mur2qtree x y dx dy n) (list2qtree n llistTerrain) )

(*
  Une fonction qui combine les deux précédentes pour faire l’intersection des quadtrees correspondant à
  chaque région intraversable   
  :signature: Quad list -> Quad
  :param: 
    - listqt : Quad list pour chaque rectangle
  :return: qtree representes par cette liste de qt 
    - avoir une list de qtree pour chaque rectangle
    - avoir l'intersection de ces qtree
*)
let rec get_terrain_from_qt qt_list = 
  match qt_list with 
  | [] -> failwith "qt list est vide"
  | qt::[] -> qt
  | qt::qqt_list -> ( inter qt (get_terrain_from_qt qqt_list) )




(****************************************************************************************
                                        Partie B   
****************************************************************************************)
(*
  numeroter les zones libres du terrain de 0 au NbdeZonesLibres-1 
  :signature: Quad -> Quad
  :param: 
    - qt : quadtree representant le terrain 
  :return: 
    - quadtree numerote   
*)
let num_quad  qt = fst (numerote qt 0 ) 



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
  let coords = Array.make k (0. ,0. )  in 
  let rec mk_coords_aux qt coords n x y = 
    let nf = float(n) in
    match qt with 
    | Mur -> ()
    | Libre (nb) -> coords.(nb) <- (x +. (nf/.2.) , y +. (nf/.2.) );
    | Quad (qt1, qt2, qt3, qt4) -> 
        mk_coords_aux qt1 coords (n/2) (x    ) (y+.nf/.2.) ;
        mk_coords_aux qt2 coords (n/2) (x+.nf/.2.) (y+.nf/.2.);
        mk_coords_aux qt3 coords (n/2) (x    ) (y    );
        mk_coords_aux qt4 coords (n/2) (x+.nf/.2.) (y    );
  in 
  mk_coords_aux qt coords n 0.  0.  ;
  coords 


(*
  afficher les coords de toutes les zones libres 
    print_coords coords k : (float*float) Array -> int -> unit 
    :param:
        - coords : array de paires de coords de chaque zones libre 
        - k : len(coords) 
    :pre-cond: k=len(coords)
*)
let print_coords coords k =
  for i = 0 to k-1 do 
    let x,y = coords.(i) in
    printf "%d -> (%.1f , %.1f) \n" i x y;
  done 
  
  




