#use "plot3d.ml";;
#use "scalarMeshFunctions.ml"
open Plot3D;;


module MazeLab = struct

    include Plot3D

    type mazeStyle = DFS | BFS | DFS_tore_x | BFS_tore_x | DFS_tore_xy | BFS_tore_xy
    type container = DFS_stack of int Stack.t | BFS_queue of int Queue.t
    
    (* random order of elements *)
    let shuffle a =
        let n = Array.length a in
            for i = n -1 downto 0 do
                let aux = a.(i) in
                    let j = Random.int (i+1) in
                        a.(i) <- a.(j);
                        a.(j) <- aux;
            done
    
    (* associates cube faces with numbers *)
    let holeOf (dx,dy,dz) = match (dx,dy,dz) with
    | (-1,0,0) -> 0
    | (1,0,0) -> 1
    | (0,-1,0) -> 2
    | (0,1,0) -> 3
    | (0,0,-1) -> 4
    | (0,0,1) -> 5
    | _ -> 0
    
    let addToArray c b =
        Array.init (Array.length b) (fun i -> c + b.(i))
            
    
    let faceRef = [|
    ([|2;0;4|],[|4;6;2|]);
    ([|1;3;7|],[|7;5;1|]);
    ([|0;1;5|],[|5;4;0|]);
    ([|3;2;6|],[|6;7;3|]);
    ([|3;1;0|],[|0;2;3|]);
    ([|4;5;7|],[|7;6;4|])|]
    
    let generalTriangleRef t = [|
    ([|t.(2);t.(0);t.(4)|],[|t.(4);t.(6);t.(2)|]);
    ([|t.(1);t.(3);t.(7)|],[|t.(7);t.(5);t.(1)|]);
    ([|t.(0);t.(1);t.(5)|],[|t.(5);t.(4);t.(0)|]);
    ([|t.(3);t.(2);t.(6)|],[|t.(6);t.(7);t.(3)|]);
    ([|t.(3);t.(1);t.(0)|],[|t.(0);t.(2);t.(3)|]);
    ([|t.(4);t.(5);t.(7)|],[|t.(7);t.(6);t.(4)|])|]
    
    (* Finds the mesh vertices of the triangles when given an edge *)
    let cubeOfCorridor n m p (i,j,k) (di,dj,dk) =
    let add = ref 0 in
        if (i+di=n) then
            add := - 8*n;
        if (i+di<0) then
            add :=  8*n;
    match (di,dj,dk) with
    | (-1,0,0) -> (addToArray (8*(i+n*j+n*m*k)) [|1-8 + !add;0;3-8 + !add;2;5-8 + !add;4;7-8 + !add;6|],[|2;3;4;5|])
    | (1,0,0) -> (addToArray (8*(i+n*j+n*m*k)) [|1;0+8 + !add;3;2+8 + !add;5;4+8 + !add;7;6+8 + !add|],[|2;3;4;5|])
    | (0,-1,0) -> (addToArray (8*(i+n*j+n*m*k)) [|2-8*n;3-8*n;0;1;6-8*n;7-8*n;4;5|],[|0;1;4;5|])
    | (0,1,0) -> (addToArray (8*(i+n*j+n*m*k)) [|2;3;0+8*n;1+8*n;6;7;4+8*n;5+8*n|],[|0;1;4;5|])
    | (0,0,-1) -> (addToArray (8*(i+n*j+n*m*k)) [|4-8*n*m;5-8*n*m;6-8*n*m;7-8*n*m;0;1;2;3|],[|0;1;2;3|])
    | (0,0,1) -> (addToArray (8*(i+n*j+n*m*k)) [|4;5;6;7;0+8*n*m;1+8*n*m;2+8*n*m;3+8*n*m|],[|0;1;2;3|])
    | _ -> ([|0;0;0;0;0;0;0;0|],[|0;0;0;0|])
    
    let trianglesOfHole n m p i j k l =
        let (a1,a2) = faceRef.(l) in
            (addToArray (8*(i+n*j + n*m*k)) a1,addToArray (8*(i+n*j + n*m*k)) a2)
    
    let cubePos l ((x,y,z) : vec3D) sz = match l with
    | 0 -> add3 (-.sz/.2.,-.sz/.2.,-.sz/.2.) (x,y,z)
    | 1 -> add3 (sz/.2.,-.sz/.2.,-.sz/.2.) (x,y,z)
    | 2 -> add3 (-.sz/.2.,sz/.2.,-.sz/.2.) (x,y,z)
    | 3 -> add3 (sz/.2.,sz/.2.,-.sz/.2.) (x,y,z)
    | 4 -> add3 (-.sz/.2.,-.sz/.2.,sz/.2.) (x,y,z)
    | 5 -> add3 (sz/.2.,-.sz/.2.,sz/.2.) (x,y,z)
    | 6 -> add3 (-.sz/.2.,sz/.2.,sz/.2.) (x,y,z)
    | 7 -> add3 (sz/.2.,sz/.2.,sz/.2.) (x,y,z)
    | _ -> (0.,0.,0.)
    
    let iob b = match b with
    |true -> 1.0
    |false -> 0.0
    
    
    let cubePos3 l (i,j,k) sz n m p border (x1,x2) (y1,y2) (z1,z2) =
    let (x,y,z) = (float_of_int(i),float_of_int(j),float_of_int(k)) in
    match l with
    | 0 -> add3 (-.sz/.2. -. border*.iob(i=0)*.x1,-.sz/.2. -. border*.iob(j=0)*.y1,-.sz/.2. -. border*.iob(k=0)*.z1) (x,y,z)
    | 1 -> add3 (sz/.2. +. border*.iob(i=n-1)*.x2,-.sz/.2. -. border*.iob(j=0)*.y1,-.sz/.2. -. border*.iob(k=0)*.z1) (x,y,z)
    | 2 -> add3 (-.sz/.2. -. border*.iob(i=0)*.x1,sz/.2. +. border*.iob(j=m-1)*.y2,-.sz/.2. -. border*.iob(k=0)*.z1) (x,y,z)
    | 3 -> add3 (sz/.2. +. border*.iob(i=n-1)*.x2,sz/.2. +. border*.iob(j=m-1)*.y2,-.sz/.2. -. border*.iob(k=0)*.z1) (x,y,z)
    | 4 -> add3 (-.sz/.2. -. border*.iob(i=0)*.x1,-.sz/.2. -. border*.iob(j=0)*.y1,sz/.2. +. border*.iob(k=p-1)*.z2) (x,y,z)
    | 5 -> add3 (sz/.2. +. border*.iob(i=n-1)*.x2,-.sz/.2. -. border*.iob(j=0)*.y1,sz/.2. +. border*.iob(k=p-1)*.z2) (x,y,z)
    | 6 -> add3 (-.sz/.2. -. border*.iob(i=0)*.x1,sz/.2. +. border*.iob(j=m-1)*.y2,sz/.2. +. border*.iob(k=p-1)*.z2) (x,y,z)
    | 7 -> add3 (sz/.2. +. border*.iob(i=n-1)*.x2,sz/.2. +. border*.iob(j=m-1)*.y2,sz/.2. +. border*.iob(k=p-1)*.z2) (x,y,z)
    | _ -> (0.,0.,0.)
    
    
    let isInside n m p (x,y,z) =
        (x<n) && (y<m) && (z<p) && (x>=0) && (y>=0) && (z>=0)
    
    (* Main function to create a maze with DFS of BFS on a tore or not *)
    let randomMaze n m p style cube_sz_ =
        let cube_sz = max 0. (min 0.999 cube_sz_) in
        print_endline "Generating maze...";
        let modulo = match style with
        | DFS -> n*m*p 
        | BFS -> n*m*p
        | DFS_tore_x -> n
        | BFS_tore_x -> n
        | DFS_tore_xy -> n
        | BFS_tore_xy -> n in
        let modulo2 = match style with
        | DFS -> n*m*p 
        | BFS -> n*m*p
        | DFS_tore_x -> n*m*p
        | BFS_tore_x -> n*m*p
        | DFS_tore_xy -> m
        | BFS_tore_xy -> m in
        let visited = Array.make (n*m*p) false
        and holes = Array.make_matrix (n*m*p) 6 false
        and corridors = Array.make (n*m*p-1) ((0,0,0),(0,0,0))
        and corridor_count = ref 0 in
        let myContainer = match style with
        | DFS -> DFS_stack (Stack.create())
        | BFS -> BFS_queue (Queue.create())
        | DFS_tore_x -> DFS_stack (Stack.create())
        | BFS_tore_x -> BFS_queue (Queue.create())
        | DFS_tore_xy -> DFS_stack (Stack.create())
        | BFS_tore_xy -> BFS_queue (Queue.create())
        and k = Random.int (n*m*p) in
            (match myContainer with
            | DFS_stack c -> Stack.push k c;
            | BFS_queue c -> Queue.push k c;);
            visited.(k) <- true;
            let len = ref (match myContainer with
            | DFS_stack c -> Stack.length c
            | BFS_queue c -> Queue.length c) in
            while(!len > 0) do
                let ind = match myContainer with
                    | DFS_stack c -> Stack.pop c
                    | BFS_queue c -> Queue.pop c in
                let (x,y,z) = (ind mod n,(ind/n) mod m,ind/(n*m))
                and choice = Array.make 6 (0,0,0) in
                    choice.(0) <- (-1,0,0);
                    choice.(1) <- (1,0,0);
                    choice.(2) <- (0,-1,0);
                    choice.(3) <- (0,1,0);
                    choice.(4) <- (0,0,-1);
                    choice.(5) <- (0,0,1);
                    shuffle choice;
                    for i = 0 to 5 do
                        let (dx,dy,dz) = choice.(i) in
                            let ind2 = ((x+dx+modulo) mod modulo) + n*((y+dy+modulo2) mod modulo2) + m*n*(z+dz) in
                                if (isInside n m p (((x+dx+modulo) mod modulo),((y+dy+modulo2) mod modulo2),z+dz) &&  not (visited.(ind2))) then begin
                                    corridors.(!corridor_count) <- ((x,y,z),(dx,dy,dz));
                                    if (abs(dx)+abs(dy)+abs(dz) > 1) then print_char ('!');
                                    corridor_count := !corridor_count + 1;
                                    holes.(ind).(holeOf (dx,dy,dz)) <- true;
                                    holes.(ind2).(holeOf (-dx,-dy,-dz)) <- true;
                                    (match myContainer with
                                    | DFS_stack c -> Stack.push ind2 c;
                                    | BFS_queue c -> Queue.add ind2 c;);
                                    visited.(ind2) <- true;
                                end
                    done;
                    len := (match myContainer with
                        | DFS_stack c -> Stack.length c
                        | BFS_queue c -> Queue.length c);
            done;
            let vertices = Array.make (8*n*m*p) (0.,0.,0.)
            and triangles = Array.make_matrix (2*4*(n*m*p-1) + 12*n*m*p - 4*(n*m*p-1)) 3 0
            and triangleCount = ref 0 in
                for i = 0 to n - 1 do
                    for j = 0 to m - 1 do
                        for k = 0 to p - 1 do
                            for l = 0 to 7 do
                                vertices.(l + 8*i + 8*n*j + 8*n*m*k) <- cubePos l (float_of_int(i),float_of_int(j),float_of_int(k)) cube_sz;
                            done
                        done
                    done
                done;
                for i = 0 to n - 1 do
                    for j = 0 to m - 1 do
                        for k = 0 to p - 1 do
                            for l = 0 to 5 do
                                if(not holes.(i + n*j + n*m*k).(l)) then begin
                                    let (arr1,arr2) = trianglesOfHole n m p i j k l in
                                        triangles.(!triangleCount) <- arr1;
                                        triangleCount := !triangleCount + 1;
                                        triangles.(!triangleCount) <- arr2;
                                        triangleCount := !triangleCount + 1;
                                end
                            done
                        done
                    done
                done;
                for kk = 0 to n*m*p - 2 do
                    let ((i,j,k),(di,dj,dk)) = corridors.(kk) in
                        let (tab,index) = cubeOfCorridor n m k (i,j,k) (di,dj,dk) in
                            let tab2 = generalTriangleRef tab in
                                for q = 0 to 3 do
                                    let (arr1,arr2) = tab2.(index.(q)) in
                                        triangles.(!triangleCount) <- arr1;
                                        triangleCount := !triangleCount + 1;
                                        triangles.(!triangleCount) <- arr2;
                                        triangleCount := !triangleCount + 1;
                                done;
                done;
                print_endline "...maze generated.";
                { nVert = 8*n*m*p;
                nTria = 2*4*(n*m*p-1) + 12*n*m*p - 4*(n*m*p-1);
                positions = vertices;
                triangles = triangles;
                colorstyle = Outside; }
                
        let randomMaze2 n m p style cube_sz_ =
        let cube_sz = max 0. (min 0.999 cube_sz_) in
        print_endline "Generating maze...";
        let graph_arr = Array.make (n*m*p) [] in
        let modulo = match style with
        | DFS -> n*m*p 
        | BFS -> n*m*p
        | DFS_tore_x -> n
        | BFS_tore_x -> n
        | DFS_tore_xy -> n
        | BFS_tore_xy -> n in
        let modulo2 = match style with
        | DFS -> n*m*p 
        | BFS -> n*m*p
        | DFS_tore_x -> n*m*p
        | BFS_tore_x -> n*m*p
        | DFS_tore_xy -> m
        | BFS_tore_xy -> m in
        let visited = Array.make (n*m*p) false
        and holes = Array.make_matrix (n*m*p) 6 false
        and corridors = Array.make (n*m*p-1) ((0,0,0),(0,0,0))
        and corridor_count = ref 0 in
        let myContainer = match style with
        | DFS -> DFS_stack (Stack.create())
        | BFS -> BFS_queue (Queue.create())
        | DFS_tore_x -> DFS_stack (Stack.create())
        | BFS_tore_x -> BFS_queue (Queue.create())
        | DFS_tore_xy -> DFS_stack (Stack.create())
        | BFS_tore_xy -> BFS_queue (Queue.create())
        and k = Random.int (n*m*p) in
            (match myContainer with
            | DFS_stack c -> Stack.push k c;
            | BFS_queue c -> Queue.push k c;);
            visited.(k) <- true;
            let len = ref (match myContainer with
            | DFS_stack c -> Stack.length c
            | BFS_queue c -> Queue.length c) in
            while(!len > 0) do
                let ind = match myContainer with
                    | DFS_stack c -> Stack.pop c
                    | BFS_queue c -> Queue.pop c in
                let (x,y,z) = (ind mod n,(ind/n) mod m,ind/(n*m))
                and choice = Array.make 6 (0,0,0) in
                    choice.(0) <- (-1,0,0);
                    choice.(1) <- (1,0,0);
                    choice.(2) <- (0,-1,0);
                    choice.(3) <- (0,1,0);
                    choice.(4) <- (0,0,-1);
                    choice.(5) <- (0,0,1);
                    shuffle choice;
                    for i = 0 to 5 do
                        let (dx,dy,dz) = choice.(i) in
                            let ind2 = ((x+dx+modulo) mod modulo) + n*((y+dy+modulo2) mod modulo2) + m*n*(z+dz) in
                                if (isInside n m p (((x+dx+modulo) mod modulo),((y+dy+modulo2) mod modulo2),z+dz) &&  not (visited.(ind2))) then begin
                                    corridors.(!corridor_count) <- ((x,y,z),(dx,dy,dz));
                                    if (abs(dx)+abs(dy)+abs(dz) > 1) then print_char ('!');
                                    corridor_count := !corridor_count + 1;
                                    holes.(ind).(holeOf (dx,dy,dz)) <- true;
                                    holes.(ind2).(holeOf (-dx,-dy,-dz)) <- true;
                                    (match myContainer with
                                    | DFS_stack c -> Stack.push ind2 c;
                                    | BFS_queue c -> Queue.add ind2 c;);
                                    visited.(ind2) <- true;
                                    graph_arr.(ind2) <- ind::(graph_arr.(ind2));
                                    graph_arr.(ind) <- ind2::(graph_arr.(ind));
                                end
                    done;
                    len := (match myContainer with
                        | DFS_stack c -> Stack.length c
                        | BFS_queue c -> Queue.length c);
            done;
            let vertices = Array.make (8*n*m*p) (0.,0.,0.)
            and triangles = Array.make_matrix (2*4*(n*m*p-1) + 12*n*m*p - 4*(n*m*p-1)) 3 0
            and triangleCount = ref 0 in
                for i = 0 to n - 1 do
                    for j = 0 to m - 1 do
                        for k = 0 to p - 1 do
                            for l = 0 to 7 do
                                vertices.(l + 8*i + 8*n*j + 8*n*m*k) <- cubePos l (float_of_int(i),float_of_int(j),float_of_int(k)) cube_sz;
                            done
                        done
                    done
                done;
                for i = 0 to n - 1 do
                    for j = 0 to m - 1 do
                        for k = 0 to p - 1 do
                            for l = 0 to 5 do
                                if(not holes.(i + n*j + n*m*k).(l)) then begin
                                    let (arr1,arr2) = trianglesOfHole n m p i j k l in
                                        triangles.(!triangleCount) <- arr1;
                                        triangleCount := !triangleCount + 1;
                                        triangles.(!triangleCount) <- arr2;
                                        triangleCount := !triangleCount + 1;
                                end
                            done
                        done
                    done
                done;
                for kk = 0 to n*m*p - 2 do
                    let ((i,j,k),(di,dj,dk)) = corridors.(kk) in
                        let (tab,index) = cubeOfCorridor n m k (i,j,k) (di,dj,dk) in
                            let tab2 = generalTriangleRef tab in
                                for q = 0 to 3 do
                                    let (arr1,arr2) = tab2.(index.(q)) in
                                        triangles.(!triangleCount) <- arr1;
                                        triangleCount := !triangleCount + 1;
                                        triangles.(!triangleCount) <- arr2;
                                        triangleCount := !triangleCount + 1;
                                done;
                done;
                print_endline "...maze generated.";
                ({ nVert = 8*n*m*p;
                nTria = 2*4*(n*m*p-1) + 12*n*m*p - 4*(n*m*p-1);
                positions = vertices;
                triangles = triangles;
                colorstyle = Outside; },graph_arr)
                
        let definePath n m p graph_arr (ii,jj,kk) style =
        let modulo = match style with
        | DFS -> n*m*p 
        | BFS -> n*m*p
        | DFS_tore_x -> n
        | BFS_tore_x -> n
        | DFS_tore_xy -> n
        | BFS_tore_xy -> n in
        let modulo2 = match style with
        | DFS -> n*m*p 
        | BFS -> n*m*p
        | DFS_tore_x -> n*m*p
        | BFS_tore_x -> n*m*p
        | DFS_tore_xy -> m
        | BFS_tore_xy -> m in
        let solver = Array.make (n*m*p) 0 in
        print_endline "Generating maze...";
        let visited = Array.make (n*m*p) false in
        let myContainer = Stack.create()
        and k = ii * n*jj + n*m*kk in
            Stack.push k myContainer;
            visited.(k) <- true;
            let len = ref (Stack.length myContainer) in
            while(!len > 0) do
                let ind = Stack.pop myContainer in
                let (x,y,z) = (ind mod n,(ind/n) mod m,ind/(n*m))
                and choice = Array.make 6 (0,0,0) in (*
                    print_int x;print_char ' ';
                    print_int y;print_char ' ';
                    print_int z;print_newline();*)
                    choice.(0) <- (-1,0,0);
                    choice.(1) <- (1,0,0);
                    choice.(2) <- (0,-1,0);
                    choice.(3) <- (0,1,0);
                    choice.(4) <- (0,0,-1);
                    choice.(5) <- (0,0,1);
                    shuffle choice;
                    for i = 0 to 5 do
                        let (dx,dy,dz) = choice.(i) in
                            let ind2 = ((x+dx+modulo) mod modulo) + n*((y+dy+modulo2) mod modulo2) + m*n*(z+dz) in
                                if ((List.mem ind2 graph_arr.(ind)) && not (visited.(ind2))) then begin
                                    if (abs(dx)+abs(dy)+abs(dz) > 1) then print_char ('!');
                                    Stack.push ind2 myContainer;
                                    visited.(ind2) <- true;
                                    solver.(ind2) <- ind;
                                end
                    done;
                    len := Stack.length myContainer;
            done;
            solver
            
    let pathMesh n m p graph_arr cube_sz_ (i1,j1,k1) (i2,j2,k2) style =
    print_char ('!');
        let solver = definePath n m p graph_arr (i1,j1,k1) style in
        print_char ('!');
        let cube_sz = max 0. (min 0.999 cube_sz_) in
        print_endline "Generating maze...";
        let res_sz = ref 1 in
        let modulo = match style with
        | DFS -> n*m*p 
        | BFS -> n*m*p
        | DFS_tore_x -> n
        | BFS_tore_x -> n
        | DFS_tore_xy -> n
        | BFS_tore_xy -> n in
        let modulo2 = match style with
        | DFS -> n*m*p 
        | BFS -> n*m*p
        | DFS_tore_x -> n*m*p
        | BFS_tore_x -> n*m*p
        | DFS_tore_xy -> m
        | BFS_tore_xy -> m in
        let visited = Array.make (n*m*p) false
        and holes = Array.make_matrix (n*m*p) 6 false
        and corridors = Array.make (n*m*p-1) ((0,0,0),(0,0,0))
        and corridor_count = ref 0 in
        let myContainer = match style with
        | DFS -> DFS_stack (Stack.create())
        | BFS -> BFS_queue (Queue.create())
        | DFS_tore_x -> DFS_stack (Stack.create())
        | BFS_tore_x -> BFS_queue (Queue.create())
        | DFS_tore_xy -> DFS_stack (Stack.create())
        | BFS_tore_xy -> BFS_queue (Queue.create())
        and k = i2 + n*j2 + m*n*k2 in
            (match myContainer with
            | DFS_stack c -> Stack.push k c;
            | BFS_queue c -> Queue.push k c;);
            visited.(k) <- true;
            let len = ref (match myContainer with
            | DFS_stack c -> Stack.length c
            | BFS_queue c -> Queue.length c) in
            while(!len > 0) do
                let ind = match myContainer with
                    | DFS_stack c -> Stack.pop c
                    | BFS_queue c -> Queue.pop c in
                let (x,y,z) = (ind mod n,(ind/n) mod m,ind/(n*m))
                and choice = Array.make 6 (0,0,0) in
                    choice.(0) <- (-1,0,0);
                    choice.(1) <- (1,0,0);
                    choice.(2) <- (0,-1,0);
                    choice.(3) <- (0,1,0);
                    choice.(4) <- (0,0,-1);
                    choice.(5) <- (0,0,1);
                    shuffle choice;
                    for i = 0 to 5 do
                        let (dx,dy,dz) = choice.(i) in
                            let ind2 = ((x+dx+modulo) mod modulo) + n*((y+dy+modulo2) mod modulo2) + m*n*(z+dz) in
                                if (solver.(ind) = ind2) then begin
                                    corridors.(!corridor_count) <- ((x,y,z),(dx,dy,dz));
                                    if (abs(dx)+abs(dy)+abs(dz) > 1) then print_char ('!');
                                    corridor_count := !corridor_count + 1;
                                    holes.(ind).(holeOf (dx,dy,dz)) <- true;
                                    holes.(ind2).(holeOf (-dx,-dy,-dz)) <- true;
                                    (match myContainer with
                                    | DFS_stack c -> Stack.push ind2 c;
                                    | BFS_queue c -> Queue.add ind2 c;);
                                    visited.(ind2) <- true;
                                    res_sz := !res_sz + 1;
                                end
                    done;
                    len := (match myContainer with
                        | DFS_stack c -> Stack.length c
                        | BFS_queue c -> Queue.length c);
            done;
            let vertices = Array.make (8*n*m*p) (0.,0.,0.)
            and triangles = Array.make_matrix (2*4*(n*m*p-1) + 12*n*m*p - 4*(n*m*p-1)) 3 0
            and triangleCount = ref 0 in
                for i = 0 to n - 1 do
                    for j = 0 to m - 1 do
                        for k = 0 to p - 1 do
                            for l = 0 to 7 do
                                vertices.(l + 8*i + 8*n*j + 8*n*m*k) <- cubePos l (float_of_int(i),float_of_int(j),float_of_int(k)) cube_sz;
                            done
                        done
                    done
                done;
                for i = 0 to n - 1 do
                    for j = 0 to m - 1 do
                        for k = 0 to p - 1 do
                            if (visited.(i + j*n + k*n*m)) then begin
                            for l = 0 to 5 do
                                if(not holes.(i + n*j + n*m*k).(l)) then begin
                                    let (arr1,arr2) = trianglesOfHole n m p i j k l in
                                        triangles.(!triangleCount) <- arr1;
                                        triangleCount := !triangleCount + 1;
                                        triangles.(!triangleCount) <- arr2;
                                        triangleCount := !triangleCount + 1;
                                end
                            done
                            end
                        done
                    done
                done;
                for kk = 0 to !res_sz - 1 do
                    let ((i,j,k),(di,dj,dk)) = corridors.(kk) in
                        let (tab,index) = cubeOfCorridor n m k (i,j,k) (di,dj,dk) in
                            let tab2 = generalTriangleRef tab in
                                for q = 0 to 3 do
                                    let (arr1,arr2) = tab2.(index.(q)) in
                                        triangles.(!triangleCount) <- arr1;
                                        triangleCount := !triangleCount + 1;
                                        triangles.(!triangleCount) <- arr2;
                                        triangleCount := !triangleCount + 1;
                                done;
                done;
                print_endline "...maze generated.";
                { nVert = 8*n*m*p;
                nTria = !triangleCount;
                positions = vertices;
                triangles = triangles;
                colorstyle = Outside; }

    let pathMesh2 n m p graph_arr cube_sz_ style =
        pathMesh n m p graph_arr cube_sz_ (0,0,0) (n-1,m-1,p-1) style
        
    let simpleEnveloppe n m p cube_sz_ (x1,x2) (y1,y2) (z1,z2) =
        let border = 1. -. cube_sz_ in
        print_endline "Generating maze...";
        let visited = Array.make (n*m*p) false in
            let vertices = Array.make (8*n*m*p) (0.,0.,0.)
            and triangles = Array.make_matrix (2*4*(n*m*p-1) + 12*n*m*p - 4*(n*m*p-1)) 3 0
            and triangleCount = ref 0 in
                for i = 0 to n - 1 do
                    for j = 0 to m - 1 do
                        for k = 0 to p - 1 do
                            for l = 0 to 7 do
                                vertices.(l + 8*i + 8*n*j + 8*n*m*k) <- cubePos3 l (i,j,k) cube_sz_ n m p border (x1,x2) (y1,y2) (z1,z2);
                            done
                        done
                    done
                done;
                for i = 0 to n - 1 do
                    for j = 0 to m - 1 do
                        for k = 0 to p - 1 do
                            if (visited.(i + j*n + k*n*m)) then begin
                            for l = 0 to 5 do
                                let on_the_border = match l with
                                | 0 -> (i=0)
                                | 1 -> (i=n-1)
                                | 2 -> (j=0)
                                | 3 -> (j=m-1)
                                | 4 -> (k=0)
                                | 5 -> (k=p-1)
                                | _ -> false in
                                if(on_the_border) then begin
                                    let (arr1,arr2) = trianglesOfHole n m p i j k l in
                                        triangles.(!triangleCount) <- arr1;
                                        triangleCount := !triangleCount + 1;
                                        triangles.(!triangleCount) <- arr2;
                                        triangleCount := !triangleCount + 1;
                                end
                            done
                            end
                        done
                    done
                done;
                print_endline "...maze generated.";
                { nVert = 8*n*m*p;
                nTria = !triangleCount;
                positions = vertices;
                triangles = triangles;
                colorstyle = Outside; }

end;;