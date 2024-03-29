# shortest-path-between-two-locations
Find the best route between two geographical locations in ocalm using graph theory and Dijkstra's algorithm

## purpose : 
We are placed in a two-dimensional field with some regions that cannot be crossed. We're looking for a path between two points that is as short as possible.


![image](https://user-images.githubusercontent.com/72779962/168636239-8bb82826-490d-4d1e-b05b-ca57ddc36f91.png)



## INPUT:
The field is a square of side n, from which we remove r impassable rectangles. It is provided as a file
text structured as follows:
1. the first line contains the number n,
2. the second line contains the number r,
3. the following r lines describe the r untraversable rectangles.
Each rectangle is described by four positive integers, given in order: the x and y coordinates of the
lower left corner, the width Dx and the height Dy. In practice, we can assume that n is a power of two.
The starting point has the coordinates (n/2, 0), the arrival point the coordinates (n/2, n). The route to be provided in response is a sequence of pairs of coordinates, such that one can go from one point to the next in a straight line without crossing an impassable zone.

The objective is to create a program which takes as input a file describing the field, and which calculates a route between the starting point and the finishing point. We will be interested in land of various sizes


# How to compile :

* Run Using the corresponding command 
* "field" is the file representing the field 

* version 1 : 
``` 
ocamlc unix.cma version1.ml  version1Test.ml  -o exe
./exe "field"
```
* version 2 
```
ocamlc unix.cma version1.ml version2.ml dijkstra.ml version2Test.ml  -o exe
./exe "fiald"
```
* The main programe
```
ocamlc unix.cma version1.ml version2.ml dijkstra.ml main.ml  -o exe
./exe "field"
```

* The optimised version: 
```
ocamlc unix.cma version1.ml version2.ml version3.ml main_optimise.ml  -o exe
./exe "field"
```



# version 1:
The field is represented by a two-dimensional array t, each cell of which corresponds to a square of side 1.
More precisely, the box ti,j contains true if the square whose lower left corner has the coordinates (i, j) is
traversable, and false otherwise. This table implicitly describes a graph:
— each box containing true is a vertex,
— the neighbors of a vertex are the adjacent free squares (there are a maximum of 4).

![image](https://user-images.githubusercontent.com/72779962/169607436-6cf0ba5e-dc9c-4251-9128-3d5bf71d556e.png)


# version 2:
The field is summarized by a quadtree. This quadtree defines a weighted graph as follows:
— each free leaf of the quadtree is a vertex,
— the neighbors of a vertex are the adjacent free regions,
— the distance between two adjacent vertices is the Euclidean distance between the centers of the two free regions
corresponding.


![image](https://user-images.githubusercontent.com/72779962/169689085-2e892d0c-a26f-406d-9615-530fc3bab79a.png)


![image](https://user-images.githubusercontent.com/72779962/168881743-139beb88-acfb-467c-b76f-a26f3c79dbe4.png)




## get Quad tree from field : 
As the indices starting from 0 

![image](https://user-images.githubusercontent.com/72779962/169688984-d2e2499a-5e3b-4323-abcd-ae91bc8deccd.png)


## get vertices coordinates 

![image](https://user-images.githubusercontent.com/72779962/169688865-76d4bd76-3ce3-478c-8d99-73c1988e899e.png)


## Get graph edges : 

![image](https://user-images.githubusercontent.com/72779962/169688821-bec316db-47bf-4386-b9ce-0feb1c8684c6.png)


## Graph from rectangle list : 

![image](https://user-images.githubusercontent.com/72779962/169688560-c876329a-21f9-488b-81ab-58fe92c82670.png)


## find distances between the graph nodes :  

![image](https://user-images.githubusercontent.com/72779962/169688726-c6468c3a-96cf-4ed5-a936-b00ded3ea40a.png)


## shortest path 
![image](https://user-images.githubusercontent.com/72779962/169716636-956dc795-1946-4dde-b86f-0ee5f4365beb.png)





