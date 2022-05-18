
(*
questions: 
q1.1: 
      Combien de cases contient le tableau t, en fonction de la taille n du terrain ? À partir de quelle valeur de 푛
      environ risquez-vous de saturer la mémoire vive de votre ordinateur ? Vous pourrez supposer que l’espace
      mémoire utilisé par un tableau de taille s est environ 8s octets
        
      1. n^2
          case => 8 oct 
          all => 8*n^2
          ram de 8gb peut avoir 8G case et donc de taille sqrt(8g)=89442 

q :

*)


(****************************************************************************************
                                        Version 1   
****************************************************************************************)


(**
   Fonction de lecture du fichier d'entrée
     load: string -> (int * int) list * int

   L'appel  (load f)  lit le fichier dont le nom est donné par la 
   chaîne (f) et renvoie une paire  (murs, n)  où
   - (murs) est la liste des quadruplets (x, y, dx, dy) donnant les
     dimensions des r zones intraversables
   - (n) est la longueur du côté du terrain
   On suppose que le fichier (f) contient une description de terrain
   valide. Le résultat de (load) n'est pas spécifié sinon.
 *)
 open Scanf
 let load file =
   let c = Scanning.open_in file in
   let n = bscanf c "%d\n" (fun n -> n) in
   let r = bscanf c "%d\n" (fun r -> r) in
   let murs = ref [] in
   for _ = 1 to r do
     bscanf c "%d %d %d %d\n" (fun x y dx dy -> murs := (x, y, dx, dy) :: !murs)
   done;
   !murs, n


(*
   fonction pour remplir un rectangle, et donc pour qu'ils soit pas traversable sur la matrice
   0 pour non traversable 
*)
let remplieRect matrix x y l h =
  for i=x to x+l- 1 do 
    for j =y to y+h-1 do
      matrix.(i).(j) <- 0; 
    done
  done
;;


(*
  une fonction qui, partant de n et de la liste des rectangles intraversables crée un tableau t représentant
  le terrain.
  return une matrice terrain a partir d'une list de rectangle 
*)
let repTerrain n listTerrain =
  let matrixRes = Array.make_matrix n n 1 in

  let rec repTerrainMatrix matrix listTerrain = 
   match listTerrain with
    [] -> ()
    |(x,y,l,h)::llistTerrain -> 
      remplieRect matrix x y l h ;
      repTerrainMatrix matrix llistTerrain
  in
  repTerrainMatrix matrixRes listTerrain ;
  matrixRes
;;

(*
  afficher une array 2d   
*)

let printMatrixWithCoord matrix n =
  for i=0 to n-1 do 
    for j =0 to n-1 do
      Printf.printf "matrix.(%d).(%d)= %d\n" i j matrix.(i).(j); 
    done
  done
;;


(*
  afficher une array 2d sous forme d'une grille  
*)

let printMatrix matrix n =
  for j=n-1 downto 0 do 
    for i =0 to n-1 do
      Printf.printf "%d " matrix.(i).(j); 
    done;
    Printf.printf "\n";
  done
;;


(* 
  :param: 
    g (int array)  -> terrain 
    vus (bol array)-> matrice des case deja visitees
    n (int)        -> longeur des ces matrices
    x (int)        -> point.x
    y (int)        -> point.y
  :return: (int * int) list de toutes les cases voisine traversable et pas encore visitees
*)
let voisin g vus n x y =
  let res = [(x,y+1);(x,y-1);(x+1,y);(x-1,y)] in 
  let rec loop res=
    match res with 
      [] -> []
      |(x,y):: rres -> 
        if ( x>=n || x<0 || y>=n || y<0)  then loop rres
        else 
          if g.(x).(y) = 0 || vus.(x).(y) = true then loop rres
          else (x,y)::(loop rres)
    in
    loop res
;; 


(*
  :param: 
    voisin_path: (bol*(int*int)list) list de path de tous les voisins 
    paire: (int*int) coord du la case actuelle 
  :return: les path ayant bol=true ou (false,[]) sinon 

  une fonction d’exploration du terrain. Cette fonction doit prendre en entrée le tableau t, les coordon  nées d’un point de départ et d’un point d’arrivée, et renvoyer un itinéraire valide sous la forme d’une liste
  de paires de coordonnées, parcour en profondeur 
*)
let rec return_valid_path voisin_path paire =
  match voisin_path with 
  [] -> []
  |list::listlist ->  paire::list 
;;

(*
  int->int->int->int->bol 
  :return: si point1 = point2
*)
let posCmp x y  x2 y2 = 
  ( (x=x2) && (y=y2) ) 
;;  


(* 
  :param: 
    terrain: int array -> matrice du terrain 
    x1 x2 y1 y2 : int->int->int->int : coord des point src et dist
  :return: valid path entre p1 et p2 
    vus: bol array -> case visitees 
    loop x y -> trouver un chemin de x y a x2 y2 , et mettre x y comme visitees
    1. trouver les voisins valides de x y 
    2. trouver un chemin entre chaque voisin et destination (true,chemin) ou (false, []) si aucun chemin 
    3. return valid_path : le premier chemin valid parmis les chemin des voisin auquel on ajout (x,y)
*)
let rec explore_find_path terrain x1 y1 x2 y2 =
  let n = Array.length terrain in
  let vus = Array.make_matrix n n false in
  let rec loop x y =
    if ( posCmp x y  x2 y2) then [(x,y)]
    else 
      begin  
        vus.(x).(y) <- true ;
        let voisin_path = List.map ( fun v ->  loop (fst v) (snd v) ) (voisin terrain vus n x y ) in
        (return_valid_path voisin_path (x,y)) 
      end 
  in
  loop x1 y1
;;


(*
  afficher (int*int) list
*)
let print_path p =
  List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) p;
  Printf.printf "\n"
;;



(*
  :return: true si (x,y) appartient a path 
  :param: 
    path (int*int) list 
    x int 
    y int 
*)
let rec path_contains path x y = 
  match path with 
  [] -> false
  | (x1,y1)::ppath -> 
    if x1 = x && y1 = y then true
      else path_contains ppath x y
;;


(*
  afficher le chemin sur la matrice comme src ***** dest   
*)
let print_path_in_terrain matrix n path =
  for j = n-1 downto 0 do 
    for i = 0 to n-1 do
      if ( path_contains path i j ) then 
        Printf.printf "* " 
      else 
        Printf.printf "%d " matrix.(i).(j); 
    done;
    Printf.printf "\n";
  done
;;




(* testing *)
(*
  u’il prenne en entrée un fchier décrivant un terrain et affche un itinéraire   
*)
let listTerrain,n = (load "terrain.txt") in 

let terrain = repTerrain n listTerrain in 

printMatrix terrain n;

Printf.printf "le chemin entre (%d,%d) et la dest (%d,%d) est : \n" 2 0 4 3;
let path = (explore_find_path terrain 2 0 4 3) in
print_path path;

Printf.printf "afficher le terrain avec chemin \n";
print_path_in_terrain terrain n path;

(* end testing *)

(****************************************************************************************
                                        Version 2   
****************************************************************************************)

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
mur2qtree: int -> int -> int -> int -> int -> qtree telle que mur2qtree x y dx dy  n
renvoie un quadtree représentant un terrain de côté n, qui est libre à l’exclusion d’un rectangle décrit par les
coordonnées (x,y) de son coin sud-ouest, sa largeur dx et sa hauteur dy
 à chaque feuille Libre on associe un nombre entier, qui permettra de l’identifer
en tant que nœud du graphe < num =-1 > par defautl 
*)

let mur2qtree x y dx dy n = 




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
  




