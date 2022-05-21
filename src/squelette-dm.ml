
  






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
