let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day24.txt") 

let maze = 
    input
    |> Seq.mapi(fun y line -> 
        line.ToCharArray()
            |> Seq.mapi(fun x c -> if c <> '#' then Some(x,y) else None))
    |> Seq.collect id
    |> Seq.choose id
    |> Set

let locations=
    input
    |> Seq.mapi(fun y line -> 
        line.ToCharArray()
            |> Seq.mapi(fun x c -> if c <> '#' && c <> '.' then Some(c |> string |> int,(x,y)) else None))
    |> Seq.collect id
    |> Seq.choose id
    |> Map

let adjacent (x,y) = 
    Set.intersect (Set [x-1,y;x+1,y;x,y+1;x,y-1]) maze

let bfs source target=
    let seen = new System.Collections.Generic.HashSet<int*int>()
    Seq.unfold(fun (frontier:Set<int*int>) -> 
        if frontier.Contains target then None
        else
        let newFrontier = 
            frontier 
            |> Seq.map adjacent
            |> Set.unionMany
            |> Set.filter(fun location -> seen.Contains location |> not)
        frontier |> Seq.iter(fun location -> seen.Add location |> ignore) 
        Some(seen.Count, newFrontier)
        ) (Set [source])
    |> Seq.length

let shortestPath = 
    seq{for locationA in locations do
        for locationB in locations do
            yield (locationA.Key, locationB.Key), bfs locationA.Value locationB.Value}
    |> Map

let shortestRoute finalDest = 
    let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

    let locs = locations |> Seq.map(fun kvp -> kvp.Key) |> Seq.toList
    let routes = permutations locs |> Seq.map(fun l -> 0::l@finalDest)
    routes
    |> Seq.map( fun route -> 
        route
        |> List.windowed 2
        |> List.map(fun l ->  shortestPath.[(l.Head, l.Tail.Head)])
        |> List.sum)
    |> Seq.min

let part1 = shortestRoute []
let part2 = shortestRoute [0]