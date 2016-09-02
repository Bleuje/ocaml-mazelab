#use "ocaml-mazelab.ml";;
#use "scalarMeshFunctions.ml";;
open MazeLab;;
open MeshFunctions;;

(* Example 1 *)
let myBasicMesh = randomMaze 10 10 10 DFS 0.8;;
Graphics.open_graph "";
writeOffMesh myBasicMesh "maze_basic.off";
Plot3D.changeCameraViewGUI 0.5 myBasicMesh Plot3D.defaultSettings;
Plot3D.plotMesh Plot3D.defaultSettings myBasicMesh;;

read_int();;

(* Example 2 *)
let szx = 100;;
let szy = 100;;
let szz = 4;;

let curve_tore (x,y,z) = (5.*.(20.+.z)*.cos(2.*.x*._pi/.float_of_int(szx)),5.*.(20.+.z)*.sin(2.*.x*._pi/.float_of_int(szx)),y*.5.);;

let curve_sphere (x,y,z) =
(5.*.(20.+.z)*.cos(2.*.x*._pi/.float_of_int(szx))*.sin(y*._pi/.float_of_int(szy)),
5.*.(20.+.z)*.sin(2.*.x*._pi/.float_of_int(szx))*.sin(y*._pi/.float_of_int(szy)),
5.*.(20.+.z)*.cos(y*._pi/.float_of_int(szy)));;

let myMesh = deformedMesh (randomMaze szx szy szz DFS_tore_x 0.3) curve_sphere;;
setColorFromValues (LinearCycleRGB (10,(1.0,0.,0.),(0.2,0.8,0.5))) bfsDepth_value myMesh;
writeOffMesh myMesh "maze_sphere.off";
Plot3D.changeCameraViewGUI 0.03 myMesh Plot3D.defaultSettings;
Plot3D.plotMesh Plot3D.defaultSettings myMesh;;

read_int();;