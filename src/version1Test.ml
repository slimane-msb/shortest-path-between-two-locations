 
open Version1 ;;

Printf.printf "testing version1\n";
let listTerrain, n = (load "terrain.txt") in 

let terrain = repTerrain n listTerrain in 

printMatrix terrain n;

Printf.printf "le chemin entre (%d,%d) et la dest (%d,%d) est : \n" 2 0 4 3;
let path = (explore_find_path terrain 2 0 4 3) in
print_path path;

Printf.printf "afficher le terrain avec chemin \n";
print_path_in_terrain terrain n path;

 
