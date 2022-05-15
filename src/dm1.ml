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



let remplieRect matrix x y l h =
  for i=x to x+l-1 do 
    for j =y to y+h-1 do
      matrix.(i).(j) <- 0; 
    done
  done
;;


(* t.(0).(1) <- 2*)
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

let printMatrix matrix n =
  for i=0 to n-1 do 
    for j =0 to n-1 do
      Printf.printf "matrix.(%d).(%d)= %d\n" i j matrix.(i).(j); 
    done
  done
;;


(* testing *)
let listTerrain,n = (load "terrain.txt") in 

let terrain = repTerrain n listTerrain in 
printMatrix terrain n 



