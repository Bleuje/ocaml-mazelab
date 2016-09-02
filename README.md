# MazeLab
Generate, view and save meshes of 3D mazes with OCaml.
This MazeLab module uses my `Plot3D` module ([repository here](https://github.com/Bleuje/ocaml-mesh-plot)).

## Quick example

``` ocaml
#use "ocaml-mazelab.ml";;
#use "scalarMeshFunctions.ml";;
open MazeLab;;
open MeshFunctions;;

let myBasicMesh = randomMaze 10 10 10 DFS 0.8;;
Graphics.open_graph "";
writeOffMesh myBasicMesh "maze_basic.off";
changeCameraViewGUI 0.5 myBasicMesh Plot3D.defaultSettings;
plotMesh Plot3D.defaultSettings myBasicMesh;;
```
Result :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mazelab/master/pictures/basic_cube.jpg)

The mesh can be saved in OFF format, and it is easy/faster to plot on MeshLab.

## Function to generate a maze
``` ocaml
let myMesh = randomMaze size_x size_y size_z DFS 0.8;;
```
This generates a maze of size size_x\*size_y\*size_z with the style `DFS` and a cube size of 0.8 (0.99 is the maximum).

## Styles of mazes
So far :
``` ocaml
type mazeStyle = DFS | BFS | DFS_tore_x | BFS_tore_x
```
You can generate a maze where the first dimension is on a tore :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mazelab/master/pictures/toreMaze.jpg)

## More advanced mazes

After the maze is generated, you can modify the positions of the vertices using deformedMesh, that's how you can have a nice visualization for a tore for example.

``` ocaml
let szx = 100;;let szy = 100;;let szz = 4;;

let curve_tore (x,y,z) =
(5.*.(20.+.z)*.cos(2.*.x*._pi/.float_of_int(szx)),
5.*.(20.+.z)*.sin(2.*.x*._pi/.float_of_int(szx)),
y*.5.);;

let curve_sphere (x,y,z) =
(5.*.(20.+.z)*.cos(2.*.x*._pi/.float_of_int(szx))*.sin(y*._pi/.float_of_int(szy)),
5.*.(20.+.z)*.sin(2.*.x*._pi/.float_of_int(szx))*.sin(y*._pi/.float_of_int(szy)),
5.*.(20.+.z)*.cos(y*._pi/.float_of_int(szy)));;

let myMesh = deformedMesh (randomMaze szx szy szz DFS_tore_x 0.3) curve_sphere;;
setColorFromValues (LinearCycleRGB (10,(1.0,0.,0.),(0.2,0.8,0.5))) bfsDepth_value myMesh;
writeOffMesh myMesh "maze_sphere.off";
changeCameraViewGUI 0.03 myMesh Plot3D.defaultSettings;
plotMesh Plot3D.defaultSettings myMesh;;
```

This projects the maze on a sphere (can take a little time)...

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mazelab/master/pictures/spheremazeplot.jpg)
![image](https://raw.githubusercontent.com/Bleuje/ocaml-mazelab/master/pictures/finishedspheremini.jpg)

## Visualization in MeshLab

So far my 3d plot doesn't work very well with objects behind the camera, but you can look inside a spherical maze by opening the OFF file with MeshLab :

![image](https://raw.githubusercontent.com/Bleuje/ocaml-mazelab/master/pictures/meshlab-insidesphere2.jpg)


