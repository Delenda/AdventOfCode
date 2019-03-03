let regionTypes max_idx_x max_idx_y target_x target_y depth= 
    let firstRow = Array.init (max_idx_x+1) (fun i-> (i*16807 + depth)%20183)
    Array.init max_idx_y (fun i -> i + 1)
    |> Array.scan (fun s y -> 
        Array.init max_idx_x (fun i -> i + 1)
        |> Array.scan(fun r x -> 
            if y = target_y && x = target_x then 
                depth%20183
            else
                ( (r * s.[x]) + depth)%20183) ( (y*48271+depth)%20183)
        ) firstRow
    |> Array.map(Array.map(fun i -> i%3))

let printRegionTypes (regionTypes: int array array) = 
    regionTypes
    |> Array.map(Array.map(fun s -> if s= 0 then "." else if s = 1 then "=" else "|"))
    |> Array.map(String.concat "")
    |> String.concat "\r\n"
    |> System.Console.WriteLine

type equipment =
    | Torch
    | Gear
    | Nothing

type location = 
    { 
        X: int
        Y: int
        Equipment : equipment
    }
    override this.ToString() = sprintf "(%d,%d) - %A" (this.X) (this.Y) (this.Equipment)

let gear regionType = 
    match regionType with
    | 0 -> [|Torch; Gear|]
    | 1 -> [|Nothing; Gear|]
    | 2 -> [|Nothing; Torch|]
    | _ -> failwith "Unexpected region type"

let getLocations (regionTypes : int array array) =
    regionTypes 
    |> Array.mapi(fun y r -> 
                    r |> Array.mapi(fun x regionType -> gear regionType |> Array.map(fun e -> {X = x; Y=y; Equipment = e})     
         ))
    |> Array.collect(Array.collect id)

let dijkstraDist (u:location) (v:location) = 
    if abs(u.X-v.X) + abs(u.Y-v.Y) > 1 then
        failwith (sprintf "Unexpected distance calculation: [%s], [%s]" (u.ToString()) (v.ToString()))
    if u = v then
        0
    else if u.X = v.X && u.Y=v.Y then
        7
    else if u.Equipment = v.Equipment then
        1
    else
        failwith (sprintf "Unexpected distance calculation: [%s], [%s]" (u.ToString()) (v.ToString()))

let deltas = [|(0,-1);(-1,0);(1,0);(0,1)|]
let neighbors (regionTypes:int array array) (u:location)=
    let max_x_idx = (regionTypes |> Array.head |> Array.length) - 1
    let max_y_idx= (regionTypes |> Array.length) - 1
    let deltaNbs = 
        deltas
        |> Array.map(fun (x,y) -> (x+u.X, y+u.Y))
        |> Array.filter(fun (x,y) -> 0 <= min x y && x <= max_x_idx && y <= max_y_idx)
        |> Array.collect(fun (x,y) -> gear (regionTypes.[y].[x]) |> Array.filter(fun e -> e = u.Equipment) |> Array.map(fun t -> {X=x;Y=y;Equipment=t} ))
    let changeEquipment = gear(regionTypes.[u.Y].[u.X]) |> Array.filter(fun e -> e <> u.Equipment) |> Array.map(fun e -> {u with Equipment = e})
    
    Array.concat [|deltaNbs;changeEquipment|] 
    |> Set

type dijkstraResult = {Vertices : Set<location>; SmallestDistance : Map<location,int>; PreviousVertex : Map<location,location>; candidates : Set<location> }
let dijkstraSearch (regionTypes : int array array) (source:location) (target:location) : dijkstraResult=
    let initialVertexSet = getLocations regionTypes |> Set
    let initialDistance = initialVertexSet |> Set.map(fun v -> if v = source then (v,0) else (v,System.Int32.MaxValue)) |> Map.ofSeq
    let initialDijkstraResult = {Vertices = initialVertexSet; SmallestDistance = initialDistance; PreviousVertex = Map.empty; candidates = Set.empty.Add(source)}
    let dijkstra (s:dijkstraResult) =
        if s.Vertices.IsEmpty then
            None
        else
            let u = s.candidates |> Seq.minBy(fun v -> s.SmallestDistance.[v]) 
            if u = target then
                None
            else
                let newVertices = s.Vertices.Remove u
                let neighbours = Set.intersect (neighbors regionTypes u) newVertices
                let newCandidates = (Set.union s.candidates neighbours).Remove u
                let distU = s.SmallestDistance.[u]
                if (distU = 1068) then
                    System.Console.WriteLine u


                let update ((dist,prev) : Map<location,int>*Map<location,location>) (v:location)  =
                    let alternateDistance = distU + (dijkstraDist u v)
                    if alternateDistance < dist.[v] then
                        let di = dist.Remove v
                        let pr = prev.Remove v
                        di.Add(v, alternateDistance), pr.Add(v,u)
                    else
                        dist,prev
                let newDistance , newPrevious = neighbours |> Set.fold update (s.SmallestDistance, s.PreviousVertex)
                let q = {Vertices = newVertices; SmallestDistance = newDistance; PreviousVertex = newPrevious; candidates = newCandidates}
                Some(q,q)
    Seq.unfold dijkstra initialDijkstraResult |> Seq.last

let rec trace dm (s: location) (a:location list)= 
    let q = dm.PreviousVertex.[a.Head]
    let newPath = q::a
    if q = s then
        newPath
    else
        trace dm s newPath

let findShortestPath x y depth = 
    let regionTypes = regionTypes (2*x) (2*y) x y depth
    let source = {X=0;Y=0;Equipment=Torch}
    let target = {X=x;Y=y;Equipment=Torch}
    let dm  = dijkstraSearch regionTypes source target
    dm.SmallestDistance.[target], trace dm source [target]

let depth, target_x, target_y = 7305, 13,734
let part1 = 
    regionTypes target_x target_y target_x target_y depth
    |> Array.sumBy Array.sum

let part2 = 
    findShortestPath target_x target_y depth |> fst