let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day24.txt") 

let rec gcd a b = 
    let x = max (abs a) (abs b)
    let y = min (abs a) (abs b)
    let r = x%y
    if r = 0 then y else gcd y r

let lcm a b = a*b/(gcd a b)

let xMax = input.[0].Length 
let yMax = input.Length 
let xPhase = xMax - 2
let yPhase = yMax - 2
let totalPhase = lcm xPhase yPhase

let neighbours (seen:Set<int*int*int>) (x0,y0,phase) = 
    let newPhase = (phase+1)%totalPhase
    seq{yield (x0+1,y0)
        yield (x0-1,y0)
        yield (x0,y0+1)
        yield (x0,y0-1)
        yield (x0,y0)}
    |> Seq.filter(fun (x,y) -> 0<=x && x<=xMax-1 && 0<=y && y<= yMax-1)
    |> Seq.filter(fun (x,y) -> input.[y].[x] <> '#' )
    |> Seq.filter(fun (x,y) -> input.[y].[(x-1+newPhase+totalPhase)%xPhase + 1] <> '<' )
    |> Seq.filter(fun (x,y) -> input.[y].[(x-1-newPhase+totalPhase)%xPhase + 1]  <> '>' )
    |> Seq.filter(fun (x,y) -> input.[(y-1+newPhase+totalPhase)%yPhase + 1].[x] <> '^' || (x,y) = (xMax-2, yMax-1))
    |> Seq.filter(fun (x,y) -> input.[(y-1-newPhase+totalPhase)%yPhase + 1].[x] <> 'v' || (x,y) = (xMax-2, yMax-1))
    |> Seq.map(fun (a,b) -> a,b,newPhase)
    |> Seq.filter(seen.Contains >> not)
    |> Set

let bfs start (xDest,yDest) = 
    Seq.unfold(fun ((frontier, seen): Set<int*int*int>*Set<int*int*int>) -> 
            let (_,_,currentPhase) = frontier |> Seq.head
            if ( frontier.Contains (xDest,yDest,currentPhase)) then None else
            let newFrontier = frontier |> Seq.map (neighbours seen) |> Set.unionMany
            let newSeen = seen + newFrontier
            let newState = newFrontier, newSeen
            Some(newState, newState)) (Set[start], Set[start])
    |> Seq.length

let heuristic (x,y,phase) (target_x,target_y) = abs(x-target_x) + abs(y-target_y)

let a_star (target:int*int) ((opened, dists): Set<int*(int*int*int)>*Map<int*int*int,int>) =
    if opened.IsEmpty then None else
    let d,((x,y,_) as current) = opened.MinimumElement
    let newOpened, newDists= 
        if (x,y) = target then
            Set.empty, Map.empty
        else
        neighbours Set.empty current
        |> Seq.fold(fun ((op,di):Set<int*(int*int*int)>*Map<int*int*int,int>) (n:int*int*int) -> 
            let newDist = dists.[current] + 1
            if di.ContainsKey n |> not then 
                op.Add(newDist + heuristic n target, n), di.Add(n,newDist)
            else if newDist < di.[n] then
                op.Remove(dists.[n] + heuristic n target, n).Add(newDist + heuristic n target, n), di.Add(n,newDist)
            else
                op,di
            ) (opened.Remove (d,current), dists)
    Some(d, (newOpened,newDists) )

let A_star_dist  (start:int*int*int) (target:int*int)= 
    Seq.unfold (a_star target) (Set [0, start], Map [start,0] ) 
    |> Seq.last

let dijkstra target  ((candidates,dists,seen): Set<int*(int*int*int)>*Map<int*int*int,int>*Set<int*int*int>) =
    if candidates.IsEmpty then None else
    let d,((x,y,_) as current)= candidates.MinimumElement
    let newCandidates, newDists = 
        if (x,y) = target then 
           Set.empty, dists
        else
        neighbours seen current
        |> Seq.fold(fun ((ca,di):Set<int*(int*int*int)>*Map<int*int*int,int>) n -> 
            let newDist = d + 1
            if dists.ContainsKey n |> not then 
                ca.Add(newDist, n), dists.Add(n,newDist)
            else if dists.[n] > newDist then
                ca.Remove(dists.[n],n).Add(newDist,n), di.Add(n,newDist)
            else
                ca,di
            ) (candidates.Remove(d,current),dists)
    let q = newCandidates, newDists, seen.Add(current)
    Some(d,q)

let dijkstraDist start target = 
    Seq.unfold (dijkstra target) (Set[0,start], Map[start,0], Set[start]) |> Seq.last

let part1 search = 
    let sw = System.Diagnostics.Stopwatch.StartNew()
    search (1,0,0) (xMax-2,yMax-1), sw.Elapsed.TotalSeconds
    
let part2 search = 
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let time1 = search (1,0,0) (xMax-2,yMax-1)
    let time2 = search (xMax-2,yMax-1,time1%totalPhase) (1,0)
    let time3 = search (1,0,(time1+time2)%totalPhase) (xMax-2,yMax - 1)
    time1 + time2 + time3, sw.Elapsed.TotalSeconds

let part1_bfs       = part1 bfs
let part2_bfs       = part2 bfs
let part1_astar     = part1 A_star_dist
let part2_astar     = part2 A_star_dist
let part1_dijkstra  = part1 dijkstraDist
let part2_dijkstra  = part2 dijkstraDist
