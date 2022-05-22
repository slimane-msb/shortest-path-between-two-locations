
  






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

  
  


