let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day12.txt") 

let links = 
    input
    |> Seq.collect(fun s -> 
        let f = s.Split('-')
        [f.[0], f.[1]; f.[1], f.[0]])
    |> Seq.groupBy fst
    |> Seq.map(fun (a,b) -> a, b |> Seq.map snd |> Set)
    |> Map

let smallCaves = 
    links
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.filter(Seq.forall System.Char.IsLower)
    |> Set

let pathSearch disallowRevisit = 
    ([["start"], Set ["start"], disallowRevisit],0)
    |> Seq.unfold(fun (paths, pathCount) -> 
        let newPaths = 
            paths
            |> List.filter(fun (c,_,_) -> c |> List.head <> "end")
            |> List.collect(fun (path, seen, disallowRevisit) -> 
                let adjacentCaves = 
                    if disallowRevisit then 
                        links.[path |> List.head] - seen 
                    else
                        links.[path |> List.head] |> Set.filter((<>) "start")
                adjacentCaves
                |> Seq.map(fun x -> 
                    let newSeen = 
                        if smallCaves.Contains x then 
                            seen |> Set.add x
                        else 
                            seen
                    x::path, newSeen, (disallowRevisit || seen.Contains x))
                |> Seq.toList)
            
        if newPaths.IsEmpty then None else
        let newPathCount = 
            newPaths
            |> List.filter(fun (c,_,_) -> c.Head = "end")
            |> List.length
            |> (+) pathCount
        Some(newPathCount, (newPaths, newPathCount))) 
    |> Seq.last
    
let part1 = pathSearch true
let part2 = pathSearch false