let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day09.txt")

let map = 
    input
    |> Array.map(fun s -> s.ToCharArray() |> Array.map string |> Array.map int)

let neighbours i j = 
   let maxI = map.Length - 1
   let maxJ = map.[0].Length - 1
   seq{ if i > 0    then yield i-1, j
        if i < maxI then yield i+1, j
        if j > 0    then yield i  , j-1
        if j < maxJ then yield i  , j+1}

let lowPoints = 
    let isLow i j = 
        neighbours i j 
        |> Seq.map(fun (x,y) -> map.[x].[y])
        |> Seq.forall(fun height -> height > map.[i].[j])
    seq{for i in [0..map.Length-1] do
        for j in [0..map.[0].Length-1] do
        if isLow i j then yield i,j}

let part1 =
    lowPoints 
    |> Seq.sumBy(fun (i,j) -> map.[i].[j] + 1)

let adjacents (i,j) = 
    neighbours i j
    |> Seq.filter(fun (i,j) -> map.[i].[j] <> 9)
    |> Set

let rec bfs (frontier : Set<int*int>) (seen : Set<int*int>) = 
    if frontier.IsEmpty then seen else
    let adjacent = frontier |> Set.map adjacents |> Set.unionMany
    let newFrontier = adjacent - seen
    let newSeen  = seen + frontier
    bfs newFrontier newSeen

let part2 =         
    let basin lowpoint = bfs (Set [lowpoint]) Set.empty
    lowPoints 
    |> Seq.map basin
    |> Seq.map Set.count
    |> Seq.sortDescending 
    |> Seq.take 3 
    |> Seq.reduce(*)