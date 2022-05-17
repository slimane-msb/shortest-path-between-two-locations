# shortest-paths-between-two-locations
Find the best route between two geographical locations in ocalm using graph theory and Dijkstra's algorithm

## OBJECTIF : 
On se place sur un terrain à deux dimensions, dont certaines zones sont intraversables. On cherche un itinéraire, si possible pas trop long, entre deux points donnés.


![image](https://user-images.githubusercontent.com/72779962/168636239-8bb82826-490d-4d1e-b05b-ca57ddc36f91.png)




## INPUT : 
Le terrain est un carré de côté n, dont on retire r rectangles intraversables. Il est fourni sous la forme d’un fichier
texte structuré ainsi :
1. la première ligne contient le nombre n,
2. la deuxième ligne contient le nombre r,
3. les r lignes suivantes décrivent les r rectangles intraversables.
Chaque rectangle est décrit par quatre nombres entiers positifs, donnés dans l’ordre : les coordonnées x et y du
coin inférieur gauche, la largeur Dx et la hauteur Dy. En pratique, on pourra supposer que n est une puissance de deux. 
Le point de départ a les coordonnées (n/2, 0), celui d’arrivée les coordonnées (n/2, n). L’itinéraire à fournir en réponse est une séquence de paires de coordonnées, telle que l’on puisse aller d’un point au suivant en ligne droite sans croiser de zone intraversable.

L’objectif  est de réaliser un programme qui prend en entrée un fichier décrivant le terrain, et qui calcule un itinéraire entre le point de départ et le point d’arrivée. On s’intéressera à des terrains de dimensions variées 



# version 1 : 
On représente le terrain par un tableau t à deux dimensions, dont chaque case correspond à un carré de côté 1.
Plus précisément, la case ti,j contient true si le carré dont le coin inférieur gauche a les coordonnées (i, j) est
traversable, et false sinon. Ce tableau décrit implicitement un graphe :
— chaque case contenant true est un sommet,
— les voisins d’un sommet sont les cases libres adjacentes (il y en a au maximum 4).

# version 2 : 
On résume le terrain par un quadtree . Ce quadtree définit un graphe pondéré de la manière suivante :
— chaque feuille libre du quadtree est un sommet,
— les voisins d’un sommet sont les régions libres adjacentes,
— la distance entre deux sommets adjacents est la distance euclidienne entre les centres des deux régions libres
correspondantes.


![image](https://user-images.githubusercontent.com/72779962/168636176-35a1315a-984e-487b-a3b7-b9633be21979.png)


![image](https://user-images.githubusercontent.com/72779962/168881743-139beb88-acfb-467c-b76f-a26f3c79dbe4.png)


![image](https://user-images.githubusercontent.com/72779962/168886417-29a1faa1-9695-46c9-8228-d34776aa74b8.png)


![image](https://user-images.githubusercontent.com/72779962/168886470-0c3b83ba-9741-4976-aeb5-2b3c3f8ef9ad.png)


![image](https://user-images.githubusercontent.com/72779962/168886650-f048c5c6-e050-4e97-8571-b67f16f574b2.png)



![image](https://user-images.githubusercontent.com/72779962/168887178-5651eceb-a53f-4094-bd72-eda53d37a8de.png)


![image](https://user-images.githubusercontent.com/72779962/168888209-9413b230-09c6-4c19-b927-5809f836a54e.png)




