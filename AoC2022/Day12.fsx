let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day12.txt")

type positions = Set<int*int>

let findPositions c : positions= 
    seq{for j in [0..input.[0].Length-1] do
        for i in [0..input.Length-1] do
            if input.[i].[j] = c then 
                yield (i,j)}
    |> Set

let start       = findPositions 'S'
let destination = findPositions 'E'
let all_a       = findPositions 'a'
        
let isNeighbour (src_row,src_col) (dst_row,dst_col) = 
    if dst_row < 0 || dst_row >= input.Length then false
    else if dst_col < 0 || dst_col >= input.[0].Length then false
    else if abs(src_row-dst_row) >= 2 || abs(src_col-dst_col) >= 2 then false
    else 
        let src = 
            if input.[src_row].[src_col] = 'S' then 'a' else input.[src_row].[src_col]
        let dst = 
            if input.[dst_row].[dst_col] = 'E' then 'z' else input.[dst_row].[dst_col]
        int(dst) - int(src) <= 1 
    
let neighbours (seen:positions) (i,j) : positions= 
    [i+1,j; i-1,j; i,j+1; i,j-1] 
    |> List.filter (isNeighbour (i,j)) 
    |> List.filter(seen.Contains >> not)
    |> Set

let shortestPath initialFrontier = 
    Seq.unfold(fun ((frontier, seen): positions*positions) -> 
            if seen.IsSupersetOf destination then None else
            let newFrontier = frontier |> Seq.map (neighbours seen) |> Set.unionMany
            let newSeen = seen + newFrontier
            let newState = newFrontier, newSeen
            Some(newState, newState)) (initialFrontier, initialFrontier)
    |> Seq.length
    
let part1 = shortestPath start
let part2 = shortestPath (all_a + start)
