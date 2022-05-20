(****************************************************************************************
                                        version 2 partie 1 TEST    
****************************************************************************************)

open Version2 ;; 
open Version1 ;;
open Printf;;

printf "******** testing version2 part 1 ********\n";

(* ***************************************************************************************
                                        q1 q2 test    
****************************************************************************************)

let qt1 = mur2qtree 2 2 2 4 8 in 
let qt2 = mur2qtree 3 1 3 1 8 in 
let inter_qt1_qt2 = inter qt1 qt2 in 

printf "\n******** qt1 : 2 2 2 4 8 ********\n\n"; print_qtree (qt1);
printf "\n******** qt1 : 3 1 3 1 8 ********\n\n"; print_qtree (qt2);
printf "\n******** inter qt1 qt2 ********\n\n"; print_qtree (inter_qt1_qt2);




(****************************************************************************************
                                        q3 test    
****************************************************************************************)

let listTerrain, n = (load "terrain3rect.txt") in 

let terrain = get_terrain n listTerrain in 

printf "\n******** Terrain a pleusieur rectangles  a partir de la list des rectangle *******\n\n"; print_qtree (terrain);

(****************************************************************************************
                                        q3 Bis test    
****************************************************************************************)
let qt3 = mur2qtree 2 4 4 1 8 in 
let terrain_qt = get_terrain_from_qt [qt1; qt2; qt3] in
printf "\n******** Terrain a pleusieur rectangles a partir des qt list *******\n\n"; print_qtree (terrain_qt);