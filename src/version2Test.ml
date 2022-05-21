(****************************************************************************************
                                        version 2 partie A TEST    
****************************************************************************************)

open Version2 ;; 
open Version1 ;;
open Printf;;

printf "******** testing version2 part 1 ********\n";

(* ***************************************************************************************
                                        A. q1 q2 test    
****************************************************************************************)

let qt1 = mur2qtree 2 2 2 4 8 in 
let qt2 = mur2qtree 3 1 3 1 8 in 
let inter_qt1_qt2 = inter qt1 qt2 in 

printf "\n******** qt1 : 2 2 2 4 8 ********\n\n"; print_qtree (qt1);
printf "\n******** qt1 : 3 1 3 1 8 ********\n\n"; print_qtree (qt2);
printf "\n******** inter qt1 qt2 ********\n\n"; print_qtree (inter_qt1_qt2);




(****************************************************************************************
                                        A.q3 test    
****************************************************************************************)

let listTerrain, n = (load "terrain3rect.txt") in 

let terrain = list2qtree n listTerrain in 

printf "\n******** Terrain a pleusieur rectangles (3 rect) : a partir de la list des rectangle *******\n\n"; print_qtree (terrain);

(****************************************************************************************
                                        A.q3 Bis test    
****************************************************************************************)
let qt3 = mur2qtree 2 4 4 1 8 in 
let terrain_qt = get_terrain_from_qt [qt1; qt2; qt3] in
printf "\n******** Terrain a pleusieur rectangles (3 rect) : a partir des qt list *******\n\n"; print_qtree (terrain_qt);


(****************************************************************************************
                                       partie B 
****************************************************************************************)

(****************************************************************************************
                                        B.q1   
****************************************************************************************)
let listTerrain, n = (load "terrain.txt") in 
let terrain = list2qtree n listTerrain in 

let terrain_num = num_quad terrain in
printf "\n******** Terrain avec numero (2rect) *******\n\n"; print_qtree (terrain_num);


(****************************************************************************************
                                        B.q2   
****************************************************************************************)
let terrain_num,k = numerote terrain 0 in 
let tab_coord = mk_coords terrain_num k n in 
printf "\n******** coords des zones libres de ce dernier *******\n\n"; print_coords tab_coord k;

(****************************************************************************************
                                        B.q3q4   
****************************************************************************************)
let g =  mk_graph terrain_num tab_coord k in 
printf "\n******** graph a 2 rect *******\n\n";print_graph g ; 
printf "\n******** graph a 2 rect -compact- *******\n\n";print_graph_compact g; 