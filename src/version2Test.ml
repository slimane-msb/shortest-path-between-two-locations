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
let listTerrain3, n = (load "terrain3rect.txt") in 
let terrain = list2qtree n listTerrain in 
let terrain3 = list2qtree n listTerrain3 in 

let terrain_num = num_quad terrain in
let terrain_num3 = num_quad terrain3 in
printf "\n******** Terrain avec numero (2rect) *******\n\n"; print_qtree (terrain_num);
printf "\n******** Terrain avec numero (3rect) *******\n\n"; print_qtree (terrain_num3);


(****************************************************************************************
                                        B.q2   
****************************************************************************************)
let terrain_num,k = numerote terrain 0 in 
let tab_coord = mk_coords terrain_num k n in 
let terrain_num3,k3 = numerote terrain3 0 in 
let tab_coord3 = mk_coords terrain_num3 k3 n in 
printf "\n******** g a 2 rect: coords des zones libres de ce dernier *******\n\n"; print_coords tab_coord k;
printf "\n******** graph a 3 rect: coords des zones libres de ce dernier *******\n\n"; print_coords tab_coord3 k3;

(****************************************************************************************
                                        B.q3q4   
****************************************************************************************)
let g =  mk_graph terrain_num tab_coord k in 
let arretes = get_all_arrete_Vnon_optimise terrain_num in 
let g3 =  mk_graph terrain_num3 tab_coord3 k3 in 
printf "\n******** arretes du graph a 2 rect *******\n\n";print_arretes arretes ; 
printf "\n******** graph a 2 rect *******\n\n";print_graph g ; 
printf "\n******** graph a 2 rect -compact- *******\n\n";print_graph_compact g; 
printf "\n******** graph a 3 rect -compact- *******\n\n";print_graph_compact g3; 



(****************************************************************************************
                                       partie C 
****************************************************************************************)

(****************************************************************************************
                                        C.q1   
****************************************************************************************)