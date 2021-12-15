let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day15.txt") 

let risklevels = 
    input
    |> Array.map(fun s -> s.ToCharArray() |> Array.map string |> Array.map int)

type location = int*int

let neighbor size ((i,j):location) =
    seq{ yield (i + 1, j)
         yield (i - 1, j)
         yield (i, j + 1) 
         yield (i, j - 1)}
    |> Seq.filter(fun loc -> loc <> (i,j) && fst loc >= 0 && snd loc >= 0 && snd loc < size && fst loc < size)

let tileSize = risklevels.Length

let risklevel ((i,j) : location) =
    let down_repeat = i/tileSize
    let right_repeat = j/tileSize
    let totalRiskLevel = risklevels.[i%tileSize].[j%tileSize] + down_repeat + right_repeat
    if totalRiskLevel > 9 then totalRiskLevel - 9 else totalRiskLevel

let dijkstra size ((Q, dist): Set<int*location>*Map<location,int>) =
    if Q.IsEmpty then None else
    let (dist_u,u) = Q.MinimumElement
    let state = 
        neighbor size u
        |> Seq.fold(fun ((q,d): Set<int*location>*Map<location,int>) v -> 
            let len = risklevel v
            let alt = dist_u + len
            let dist_v = d.TryFind v |> Option.defaultValue(alt + 1)
            if alt < dist_v then 
                (dist_v,v) |> q.Remove |> Set.add (alt,v), d.Add(v,alt)
            else
                q,d) (Q.Remove(dist_u,u),dist)
    Some(snd state,state)
    
let shortestPath size = 
    let initial_dist = Map [(0,0),0]
    let initial_PriorityQueue = Set [0, (0,0)]
    let dists = Seq.unfold (dijkstra size) (initial_PriorityQueue, initial_dist) |> Seq.last
    dists.[size-1,size-1]

let part1 = shortestPath tileSize
let part2 = shortestPath (5*tileSize)