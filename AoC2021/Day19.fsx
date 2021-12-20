let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day19.txt") 

type point = {x:int;y:int;z:int}
let add p1 p2 = {x = p1.x + p2.x; y = p1.y + p2.y; z = p1.z + p2.z}

let parsePoint (line:string)=
    let f = line.Split(',') |> Array.map int
    {x=f.[0]; y=f.[1];z=f.[2]}

let scanners = 
    input
    |> Array.filter(fun x -> x.Length <> 0)
    |> Seq.fold(fun (scanners,beacons) (line:string) ->
        if line.Contains "--" then 
            (List.sort beacons)::scanners, []
        else
            scanners,(parsePoint line)::beacons) ([],[])
    |> fun s -> snd s :: fst s
    |> List.rev
    |> List.tail
    |> List.indexed
    |> Map

let offsets (points: point list) = 
    seq{for p1 in points do
        for p2 in points do 
            yield {x= p2.x - p1.x; y= p2.y - p1.y; z = p2.z - p1.z}}
    |> Set
    |> Set.remove {x=0;y=0;z=0}

let deltas = scanners |> Map.map(fun scanner beacons -> offsets beacons)


let s4 = 
    let rx point = {x = point.x; y = point.z; z = -point.y}
    let ry point = {x = point.z; y = point.y; z = -point.x}
    let rz point = {x = -point.y; y = point.x; z = point.z}
    
    [id; rz; rz>>rz; rz>>rz>>rz] 
    |> List.collect(fun s -> 
        [id; ry; ry>>ry; ry>>ry>>ry; rx; rx>>rx>>rx] 
        |> List.map(fun t -> t >> s))

let orientations  =
    seq{for s1 in [0..(scanners.Count - 1)] do
        for s2 in [0..(scanners.Count - 1)] do
        for sigma in s4 do
        if (deltas.[s2]) |> Set.map sigma |> Set.intersect (deltas.[s1]) |> Seq.length >= 132 then
            if s1 <> s2 then 
                yield (s1,s2), sigma }
    |> Map

let pointPairs (points: point list) = 
    seq{for p1 in points do
        for p2 in points do 
            yield {x= p2.x - p1.x; y= p2.y - p1.y; z = p2.z - p1.z}, (p1,p2)}
    |> Seq.filter(fun (delta, _) -> delta <> {x=0;y=0;z=0})
    |> Set

let delta (baseScanner, offsetScanner) = 
    let orientation = orientations.[baseScanner, offsetScanner]
    let beacons1 = scanners.[baseScanner]
    let beacons2 = scanners.[offsetScanner] |> List.map orientation
    let offsets1 = pointPairs beacons1
    let offsets2 = pointPairs beacons2
    let commonDeltas = Set.intersect (offsets1 |> Set.map fst) (offsets2 |> Set.map fst)
    let col1 = offsets1 |> Seq.filter(fun offset -> commonDeltas.Contains (fst offset)) |> Seq.map snd |> Seq.collect(fun x -> [fst x ; snd x]) |> Seq.distinct |> Seq.toList
    let col2 = offsets2 |> Seq.filter(fun offset -> commonDeltas.Contains (fst offset)) |> Seq.map snd |> Seq.collect(fun x -> [fst x ; snd x]) |> Seq.distinct |> Seq.toList
    let m1 = col1 |> List.sort |> List.head
    let m2 = col2 |> List.sort |> List.head
    {x = m1.x - m2.x; y = m1.y - m2.y; z = m1.z - m2.z}

let links = 
    orientations
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.groupBy fst
    |> Seq.map(fun (x,y) -> x, y |> Seq.map snd |> Set)
    |> Map

let rec findPaths (front:Set<int>) (ps:Map<int,int list>)=
    if front.IsEmpty then ps else
    let s = front.MaximumElement
    let nextS = links.[s] |> Set.filter(fun d -> ps.ContainsKey d |> not)
    let newPs = nextS |> Seq.fold(fun (u:Map<int,int list>) t -> u.Add(t, t::ps.[s])) ps
    let newFront = nextS + (front.Remove s)
    findPaths newFront newPs

let paths = findPaths (Set [0]) (Map [0, [0]]) |> Map.map (fun _ p -> List.rev p) |> Map.remove 0

let alignments = 
    paths
    |> Map.map(fun _ path -> 
        path
        |> List.windowed 2
        |> List.rev
        |> List.map(fun x -> orientations.[x.Head, x.Tail.Head])
        |> List.reduce (>>))
    |> fun m -> m.Add(0, id)

let absolutePositions = 
    paths
    |> Map.map(fun _ path ->
        path
        |> List.windowed 2
        |> List.rev
        |> List.map(fun x -> delta (x.Head, x.Tail.Head) |> alignments.[x.Head])
        |> List.reduce add)
    |> fun m -> m.Add(0, {x=0;y=0;z=0})

let part1 = 
    scanners 
    |> Map.map(fun scanner beacons -> 
        beacons 
        |> List.map(alignments.[scanner]) 
        |> List.map(add absolutePositions.[scanner])
        |> Set)
    |> Map.toSeq
    |> Seq.map snd
    |> Set.unionMany
    |> Set.count

let part2 = 
    let scannerLocations = absolutePositions |> Map.toSeq |> Seq.map snd |> Seq.toList
    seq{for s1 in scannerLocations do
        for s2 in scannerLocations do
        yield abs (s1.x - s2.x) + abs (s1.y-s2.y) + abs (s1.z-s2.z)}
    |> Seq.max