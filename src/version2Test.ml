open Version2 ;; 
open Version1 ;;
open Printf;;



printf "testing version2 part 1\n";
(* let listTerrain, n = (load "terrain.txt") in 

let terrain = repTerrain n listTerrain in 

printMatrix terrain n; *)

(* test qtree*)
let qt1 = mur2qtree 2 2 2 4 8 in 
let qt2 = mur2qtree 3 1 3 1 8 in 
let inter_qt1_qt2 = inter qt1 qt2 in 

printf "\nqt1 : 2 2 2 4 8\n\n"; print_qtree (qt1);
printf "\nqt1 : 3 1 3 1 8\n\n"; print_qtree (qt2);
printf "\ninter qt1 qt2\n\n"; print_qtree (inter_qt1_qt2);

