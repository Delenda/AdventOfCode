let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day22.txt") 

type coordinate = int*int
let manhattanDist ((x,y):coordinate) ((z,w):coordinate) = abs(x-z) + abs(y-w)
type Vertex = int
type Face = Vertex list

let movements = input |> Seq.last
let map = input |> Seq.rev |> Seq.skip 2 |> Seq.rev |> Seq.toArray
let mapWidth = map |> Seq.map(fun s -> s.Length) |> Seq.max
let edgeLength = mapWidth*map.Length/12 |> float |> System.Math.Sqrt |> int

let rightBorder = map |> Seq.mapi(fun i s -> i, s.Length - 1) |> Map
let leftBorder = map |> Seq.mapi(fun i s -> i,s.ToCharArray() |> Seq.takeWhile(fun c -> c = ' ') |> Seq.length) |> Map
let verticalBorder pick = 
    seq{ for col in [0.. (mapWidth-1)] do
            let b =
                map
                |> Seq.indexed
                |> Seq.filter( fun (_,s) -> s.Length > col)
                |> Seq.filter(fun (_,s) -> s.[col] <> ' ')
                |> Seq.map fst
                |> pick
            yield col,b
            } |> Map
let upperBorder = verticalBorder Seq.min
let lowerBorder = verticalBorder Seq.max

type command = 
    | Forward 
    | Right
    | Left

let parseCommand (tokens:char list) = 
    match tokens with
    | 'R'::_ -> [Right], tokens.Tail
    | 'L'::_ -> [Left], tokens.Tail
    | _ -> 
        let digits = tokens |> List.takeWhile(System.Char.IsDigit)
        let n = digits |> Seq.map string |> String.concat "" |> int 
        List.replicate n Forward, tokens |> List.skip digits.Length

let rec parseCommands (tokens:char list) commandList= 
    if tokens.IsEmpty then commandList else
    let cmd,rest = parseCommand tokens
    parseCommands rest (commandList@cmd)
    
let commands = parseCommands (movements.ToCharArray() |> Seq.toList) [] 

let move (x,y,dir) =
    let newX,newY = 
        match dir with 
        | 0 -> 
            if x = rightBorder.[y] then 
                leftBorder.[y],y
            else
                x + 1,y
        | 2 ->
            if x = leftBorder.[y] then 
                rightBorder.[y],y
            else
                x - 1,y
        | 1 -> 
            if y = lowerBorder.[x] then
                x, upperBorder.[x]
            else
                x, y + 1
        | 3 -> 
            if y = upperBorder.[x] then
                x, lowerBorder.[x]
            else
                x, y - 1            
        | _ -> failwith "unexpected dir"
    if map.[newY].[newX] <> '#' then 
        let q = newX,newY,dir
        Some(q,q)
    else 
        let q = x,y,dir
        Some(q,q)

let step ((x,y,dir):int*int*int) (cmd:command) =
    match cmd with
    | Right -> x,y,(dir + 1)%4
    | Left  -> x,y,(dir + 3)%4
    | Forward -> 
        Seq.unfold move (x,y,dir) |> Seq.take 1 |> Seq.last

let face x y = 
    if x >= 100 && y<50 then 4 else
    if x >= 50  && y<50 then 1 else
    if y < 100 then 2 else
    if x < 50 && y < 150 then 3 else
    if y < 150 then 6 else
    if x < 50 && y >= 150 then 5 
    else
        failwith "unexpected face location"

// hardcoded to specific input
let moveCube (x,y,dir) cmd = 
    let newX, newY, newDir =
        match cmd with
        | Right -> x,y,(dir + 1)%4
        | Left  -> x,y,(dir + 3)%4
        | Forward -> 
            match dir with
            | 0 -> 
                if x = rightBorder.[y] then 
                    match face x y with 
                    | 2 ->  50 + y,49,3
                    | 6 ->  149, 149-y, 2
                    | 4 ->  99, 149-y,2
                    | 5 ->  y - 100,149, 3
                    | _ -> failwith "unexpected face going right"
                else
                    x + 1,y,dir
            | 1 -> 
                if y = lowerBorder.[x] then
                    match face x y with 
                    | 4 -> 99,x-50, 2
                    | 6 -> 49,100+x, 2
                    | 5 -> x+100,0,1
                    | _ -> failwith "unexpected face going down"
                else
                    x, y + 1,dir
            | 2 -> 
                if x = leftBorder.[y] then 
                    match face x y with
                    | 2 -> y-50,100,1
                    | 1 -> 0,149-y,0
                    | 3 -> 50,149-y,0
                    | 5 -> y-100,0,1
                    | _ -> failwith "unexpected face going left"
                else
                    x - 1,y,dir
            | 3 -> 
                if y = upperBorder.[x] then
                    match face x y with
                    | 3 -> 50,50+x,0
                    | 1 -> 0,x+100,0
                    | 4 -> x-100,199,3
                    | _ -> failwith "unexpected face going up"
                else
                    x, y - 1,dir      
            | _ -> failwith "unexpected dir"
    if map.[newY].[newX] <> '#' then 
        newX,newY,newDir
    else 
        x,y,dir

let part1 = 
    let start_x = upperBorder |> Map.toSeq |> Seq.filter(fun (x,y) -> y = 0) |> Seq.map fst |> Seq.min
    let start_y = 0
    let start_dir = 0
    let end_x, end_y,end_dir = commands |> List.fold step (start_x,start_y,start_dir)
    1000*(end_y+1) + 4*(end_x+1) + end_dir
    
// Solution to part2 with hardcoding specific to input
//let part2 = 
//    let start_x = upperBorder |> Map.toSeq |> Seq.filter(fun (x,y) -> y = 0) |> Seq.map fst |> Seq.min
//    let start_y = 0
//    let start_dir = 0
//    let end_x, end_y,end_dir = commands |> List.fold moveCube (start_x,start_y,start_dir)
//    1000*(end_y+1) + 4*(end_x+1) + end_dir

let cube = 
    let face1 : Face = [1;2;3;4]
    let face2 : Face = [8;7;6;5]
    let face3 : Face = [2;1;5;6]
    let face4 : Face = [3;2;6;7]
    let face5 : Face = [4;3;7;8]
    let face6 : Face = [1;4;8;5]
    
    let edge2facepairMap = 
        let faces = [face1;face2;face3;face4;face5;face6]
        seq{for face in faces do 
            for otherface in faces do
            let edge = Set.intersect (Set face) (Set otherface)
            if edge.Count = 2 then 
                yield edge, Set[face;otherface] }
        |> Map
    
    let unfoldedCubeFaces = 
        seq{for col in [0 .. edgeLength .. mapWidth-edgeLength] do
            for row in [0 .. edgeLength .. map.Length-edgeLength] do
            if col < map.[row].Length then
                yield row,col}
        |> Seq.filter(fun (j,i) -> map.[j].[i] <> ' ')
        |> Set
    
    let coordinateEdges = 
        unfoldedCubeFaces
        |> Seq.map(fun (upperLeft_row,upperLeft_col) -> 
            let corners =
                [
                    upperLeft_row               , upperLeft_col
                    upperLeft_row + edgeLength  , upperLeft_col
                    upperLeft_row + edgeLength  , upperLeft_col + edgeLength
                    upperLeft_row               , upperLeft_col + edgeLength
                ] 
            let edges = ((List.rev corners).Head,corners.Head)::(List.windowed 2 corners |> List.map(fun l -> l.Head, l.Tail.Head))
            (upperLeft_row,upperLeft_col),edges
            )
        |> Map
    
    let rec foldCube (vertex2coordinateMap: Map<Face,Map<Vertex, coordinate>> ) (face:Face)=
        let coordinateMap = vertex2coordinateMap.[face]
        let neighbors = 
            let upperLeftCoordinate =
                coordinateMap |> Map.toSeq |> Seq.map snd |> Seq.min
            unfoldedCubeFaces
            |> Seq.filter(fun x -> manhattanDist x upperLeftCoordinate = edgeLength)
            |> Set
            |> Set.filter(fun coord -> 
                let edges = 
                    coordinateEdges.[coord]
                    |> List.map(fun (a,b) -> Set[a;b])
                    |> Set.unionMany
                vertex2coordinateMap
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.filter(fun m -> 
                    let cs = m |> Map.toSeq |> Seq.map snd |> Set
                    cs = edges)
                |> Seq.isEmpty
            )
        if neighbors.IsEmpty then vertex2coordinateMap else
        neighbors
        |> Seq.fold(fun s t -> 
            let edges = coordinateEdges.[t]
            let edgesCoords = 
                edges
                |> Seq.map(fun (a,b) -> Set[a;b])
                |> Set.unionMany
            let edge = 
                coordinateMap 
                |> Map.filter(fun k v -> edgesCoords.Contains v)
                |> Map.toSeq
                |> Seq.map fst
                |> Set
            let newFace = edge2facepairMap.[edge].Remove(face) |> Seq.exactlyOne
    
            let edgeCoord = edge |> Seq.map(fun d -> coordinateMap.[d]) |> Set
    
            let idx = edges |> List.findIndex(fun t -> Set[fst t; snd t] = edgeCoord)
            let a,b = edges |> List.splitAt idx
            let edgesReordered = b@a
            let faceReordered = 
                let idx = 
                    newFace 
                    |> List.windowed 2 
                    |> List.tryFindIndex(fun d -> Set d  = edge)
                    |> Option.defaultValue 3
                let a,b = newFace |> List.splitAt idx
                b@a
            let newCoordinateMap = 
                List.zip faceReordered edgesReordered
                |> List.map(fun (a,b) -> a, fst b)
                |> Map
            foldCube (s.Add(newFace, newCoordinateMap)) newFace
            ) vertex2coordinateMap

    let m = 
        let initialFace = face1
        let initialCoordMap = 
            let start_x = upperBorder |> Map.toSeq |> Seq.filter(fun (x,y) -> y = 0) |> Seq.map fst |> Seq.min
            let start_y = 0
            let square = unfoldedCubeFaces |> Seq.filter(fun x -> x = (start_y,start_x)) |> Seq.head
            let edges  = coordinateEdges.[square]
            List.zip initialFace edges
            |> List.map(fun (a,b) -> a, fst b)
            |> Map
        
        let initialMap = Map[initialFace,initialCoordMap]
        
        foldCube initialMap initialFace
    
    let buildFace face ((dx1,dy1,dz1):int*int*int) ((dx2,dy2,dz2):int*int*int) ((x_0,y_0,z_0):int*int*int) (vertex:Vertex)= 
        let prev = face@face |> List.windowed 2 |> List.filter(fun l -> l.Tail.Head = vertex) |> List.take 1 |> List.exactlyOne |> List.head
        let next = face@face |> List.windowed 2 |> List.filter(fun l -> l.Head = vertex) |> List.take 1 |> List.exactlyOne |> List.skip 1 |> List.exactlyOne
    
        let (cr,cc) = m.[face].[vertex]
        let (pr,pc) = m.[face].[prev]
        let (nr,nc) = m.[face].[next]
    
        let (dir_1r,dir_1c) = (pr - cr)/edgeLength, (pc - cc)/edgeLength
        let (dir_2r,dir_2c) = (nr - cr)/edgeLength, (nc - cc)/edgeLength
        let (corner_r,corner_c) = 
            (if cr = max pr nr then cr-1 else cr), (if cc = max pc nc then cc-1 else cc)
        seq{ for d1 in [0..edgeLength-1] do
             for d2 in [0..edgeLength-1] do
             yield (x_0 + dx1*d1 + dx2*d2,y_0 + dy1*d1 + dy2*d2,z_0 + dz1*d1 + dz2*d2), (corner_r + d2*dir_2r + d1*dir_1r, corner_c + d1*dir_1c + d2*dir_2c)
             }
        |> Set

    [
        buildFace face1 ( 1, 0, 0) ( 0,-1, 0) (1,           edgeLength,     edgeLength+1)   2
        buildFace face2 (-1, 0, 0) ( 0,-1, 0) (edgeLength,  edgeLength,     0)              5 
        buildFace face3 (-1, 0, 0) ( 0, 0,-1) (edgeLength,  edgeLength+1,   edgeLength)     1
        buildFace face4 ( 0,-1, 0) ( 0, 0,-1) (0,           edgeLength,     edgeLength)     2
        buildFace face5 ( 1, 0, 0) ( 0, 0,-1) (1,           0,              edgeLength)     3
        buildFace face6 ( 0, 1, 0) ( 0, 0,-1) (edgeLength+1,1,              edgeLength)     4
    ] |> Set.unionMany |> Map

let Face (x,y,z)=
    if z = edgeLength+1 then Some 1 else
    if z = 0 then Some 2 else
    if x = 0 then Some 4 else
    if x = edgeLength+1 then Some 6 else
    if y = 0 then Some 5 else
    if y = edgeLength + 1 then Some 3
    else None

let moveFoldedCube (x,y,z,dir) (cmd:command) =
    let r_x (x,y,z) = x, z,-y // rotate towards back
    let r_y (x,y,z) = -z,y,x  // rotate towards left
    let r_z (x,y,z) = -y,x,z  // rotate towards left

    let p = (x,y,z)
    match cmd with
    | Forward -> 
        let (dx,dy,dz) = dir
        let x0,y0,z0 = x+dx,y+dy,z+dz
        let x1,y1,z1,dir1  =
            if cube.ContainsKey (x0,y0,z0) then
                x0,y0,z0,dir
            else 
                match Face p with
                | Some 1 -> x0,   y0,   z0-1, ( 0, 0,-1)
                | Some 2 -> x0,   y0,   z0+1, ( 0, 0, 1)
                | Some 3 -> x0,   y0-1, z0,   ( 0,-1, 0)
                | Some 4 -> x0+1, y0,   z0,   ( 1, 0, 0)
                | Some 5 -> x0,   y0+1, z0,   ( 0, 1, 0)
                | Some 6 -> x0-1, y0,   z0,   (-1, 0, 0)
                | _ -> failwith "unexpected"
        let r,c = cube.[x1,y1,z1]
        if map.[r].[c] = '#' then 
            x,y,z,dir
        else
            x1,y1,z1,dir1
    | Left ->
            match Face p with
            | Some 1 -> x,y,z, r_z dir
            | Some 2 -> x,y,z, (r_z>>r_z>>r_z) dir
            | Some 3 -> x,y,z, (r_y>>r_y>>r_y) dir
            | Some 4 -> x,y,z, r_x dir
            | Some 5 -> x,y,z, r_y dir
            | Some 6 -> x,y,z, (r_x>>r_x>>r_x) dir
            | _ -> failwith "unexpected"
    | Right ->
            match Face p with
            | Some 1 -> x,y,z, (r_z>>r_z>>r_z) dir
            | Some 2 -> x,y,z, r_z dir
            | Some 3 -> x,y,z, r_y dir
            | Some 4 -> x,y,z, (r_x>>r_x>>r_x) dir
            | Some 5 -> x,y,z, (r_y>>r_y>>r_y) dir
            | Some 6 -> x,y,z, r_x dir
            | _ -> failwith "unexpected"
    
let part2_cube = 
    let (x0,y0,z0,(dx,dy,dz)) = commands |> List.fold moveFoldedCube (1,edgeLength,edgeLength+1,(1,0,0))
    let r,c = cube.[x0,y0,z0]
    let dir = 
        let x1,y1,z1 = x0+dx,y0+dy,z0+dz
        let dr,dc =
            if cube.ContainsKey(x1,y1,z1) then
                let r1,c1 = cube.[x1,y1,z1]
                let r0,c0 = cube.[x0,y0,z0]
                (r1-r0), (c1-c0)
            else //just assuming the last command wasn't a turn...
                let r1,c1 = cube.[x0-dx,y0-dy,z0-dz]
                let r0,c0 = cube.[x0,y0,z0]
                (r0-r1),(c0-c1)
        match (dr,dc) with
        | (0,1) -> 0
        | (1,0) -> 1
        | (0,-1) -> 2
        | (-1,0) -> 3
        | _ -> failwith "unexpected direction"

    1000*(r+1) + 4*(c+1) + dir
