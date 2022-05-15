(***********************************************************************
 *  Outils logiques et alorithmiques,
 *  Thibaut Balabonski @ Université Paris-Saclay
 *  L2 Info, printemps 2022
 *
 *  Éléments de code pour le DM.
 *  
 *  Note : ces éléments sont regroupés dans un seul fichier, mais il
 *  peut-être judicieux de les séparer en plusieurs modules. 
 *  Par exemple : avoir un fichier pour le programme principal et des 
 *  modules à part pour  les quadtrees, les graphes, et chaque autre 
 *  structure de données.
 * 
 *  Certaines fonctions ne sont pas implémentées, et servent juste à
 *  proposer une ossature de programme complet. N'hésitez pas à
 *  modifier la signature de ces fonctions pour personnaliser la
 *  structure de votre projet.
 **********************************************************************)

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
  

(**
   Type pour représenter un graphe pondéré dans la partie 2 : 
   tableau de listes d'adjacence

   Sous-entendu : les sommets d'un graphe sont numérotés consécutivement à
   partir de 0. Un graphe (g) et un numéro de sommet (i) étant donnés, g.(i)
   est une liste de paires, où chaque paire (v, d) contient
   - le numéro (v) d'un voisin
   - la distance (d) à ce voisin

   Deux fonctions d'affichage fournies
   (print_graph) donne une vue complète, longueurs des arêtes comprises
   (print_graph_compact) 
 *)
type graph = (int * float) list array

let print_graph g =
  let n = Array.length g in
  printf "Graphe à %d sommets :\n" n;
  for i = 0 to n-1 do
    printf "Sommet %d:\n" i;
    List.iter (fun (v, d) -> printf "  voisin %d à distance %f\n" v d) g.(i)
  done

let print_graph_compact g =
  for i = 0 to Array.length g - 1 do
    printf "%d:" i;
    List.iter (fun (v, _) -> printf " %d" v) g.(i);
    printf "\n"
  done

(**
   Fonction de construction d'un graphe pondéré à partir d'un quadtree.
   Renvoie un graphe.

   Arguments :
   - (qt) le quadtree
   - (coords) le tableau des coordonnées
 *)
let mk_graph qt coords =
  failwith "not implemented"


(**
   Fonction de recherche d'un chemin.
   Applique un algorithme de recherche et reconstruit le trouvé.
   Renvoie la liste des paires de coordonnées formant le chemin.

   Arguments :
   - (xDep, yDep) les coordonnées du point de départ
   - (xArr, yArr) les coordonnées du point d'arrivée
   - (qt, n) le quadtree et la longueur du côté du terrain
   - (g, coords) le graphe et le tableau des coordonnées
 *)
let find_path (xDep, yDep) (xArr, yArr) (qt, n) (g, coords) =
  failwith "not implemented"

let print_path p =
  List.iter (fun (x, y) -> printf "(%d, %d) " x y) p;
  printf "\n"

  
  
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

(**
   Exemple d'ossature pour un programme complet, prenant un nom de fichier
   sur la ligne de commande et affichant le chemin trouvé, ainsi que quelques
   mesures de temps d'exécution.
   Note : la mesure des temps d'exécution peut ne pas fonctionner sous windows.
 *)
  
let _ =
  let file = Sys.argv.(1) in
  let murs, n = load file in
  let t1 = Unix.gettimeofday() in
  let qt, k = numerote (list2qtree n murs) 0 in
  let t2 = Unix.gettimeofday() in
  let coords = mk_coords qt k n in
  let g = mk_graph qt coords in
  let t3 = Unix.gettimeofday() in
  let path = find_path (n/2, 0) (n/2, n) (qt, n) (g, coords) in
  let t4 = Unix.gettimeofday() in
  printf "Temps:\n  construction du quadtree %fs\n  construction du graphe %fs\n  recherche de chemin %fs\n" (t2 -. t1) (t3 -. t2) (t4 -. t3);
  print_path path
