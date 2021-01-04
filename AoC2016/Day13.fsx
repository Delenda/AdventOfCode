let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day13.txt") |> int

let adjacent favoriteNumber (x0,y0) = 
    [x0+1,y0;x0-1,y0;x0,y0+1;x0,y0-1]
    |> List.filter(fun (x,y) -> x>=0 && y>= 0)
    |> List.filter(fun (x,y) -> 
        let total = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber
        let binary =
            System.Convert.ToString(total, 2)
                .ToCharArray()
                |> Array.filter(fun c -> c = '1')
                |> Seq.length  
        binary%2 = 0)
    |> Set

let bfs favoriteNumber target=
    let seen = new System.Collections.Generic.HashSet<int*int>()
    Seq.unfold(fun (frontier:Set<int*int>) -> 
        if frontier.Contains target then None
        else
        let newFrontier = 
            frontier 
            |> Seq.map (adjacent favoriteNumber)
            |> Set.unionMany
            |> Set.filter(fun location -> seen.Contains location |> not)
        frontier |> Seq.iter(fun location -> seen.Add location |> ignore) 
        Some(seen.Count, newFrontier)
        ) (Set [(1,1)])

let part1 = bfs input (31,39) |> Seq.length
let part2 = bfs input (31,39) |> Seq.take 51 |> Seq.last    