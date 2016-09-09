#use "ocaml-mazelab.ml";;
#use "scalarMeshFunctions.ml";;
open MazeLab;;
open MeshFunctions;;

Graphics.open_graph "";;

let szx1 = 200;;
let szy1 = 50;;
let szz1 = 1;;

let myMaze1 = randomMaze szx1 szy1 szz1 DFS_tore_xy 0.85;;

let name = "maze_tore3";;

let radius_x = float_of_int(szx1)/.(2.*._pi);;
let radius_y = float_of_int(szy1)/.(2.*._pi);;

let f (x,y,z) =
    let t = x/.float_of_int(szx1) in
    let t2 = y/.float_of_int(szy1) in
    let r = radius_x +. (radius_y+.z)*.cos(2.*._pi*.t2) in
    let xx = r*.cos(2.*._pi*.t) in
    let yy = r*.sin(2.*._pi*.t) in
    let zz = (radius_y+.z)*.sin(2.*._pi*.t2) in
    (xx,yy,zz);;
    
let myMaze = deformedMesh (movedMesh myMaze1 (0.,0.,-.0.15)) f;;
    

writeOffMesh myMaze ("off/"^name^".off");
print_int(Sys.command("meshlabserver -i off/"^name^".off -o ply/"^name^".ply"));
Plot3D.changeCameraViewGUI 0.1 myMaze Plot3D.defaultSettings;
Plot3D.plotMesh Plot3D.defaultSettings myMaze;;

read_int();;
