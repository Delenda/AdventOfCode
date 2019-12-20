let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day20.txt") 

let cells = 
    Array.append (Array.replicate 1 "") input
    |> fun d -> Array.append d (Array.replicate 1 "")
    |> Array.map(fun s -> s.PadLeft(128,' ').PadRight(129,' ').ToCharArray() |> Array.map string)
 
let height = cells.Length
let width = 129

let isLetter s = s<>" "&& s<>"." && s<>"#"

let labeledPoints = 
    seq{ for x in [1..(width - 2)] do
         for y in [1..(height - 2)] do
         let cell = cells.[y].[x]
         if isLetter cell then
            let leftCell = cells.[y].[x-1]
            let rightCell = cells.[y].[x+1]
            let upperCell = cells.[y-1].[x]
            let lowerCell = cells.[y+1].[x]
            if isLetter rightCell then
                let label = cell + rightCell
                if leftCell = "." then
                    yield (label, (x-1,y))
                else
                    yield (label, (x+2,y))
            if isLetter upperCell then
                let label = upperCell + cell
                if lowerCell = "." then
                    yield (label, (x,y+1))
                else
                    yield (label, (x,y-2))
         }
    |> Seq.toList


let start = labeledPoints |> List.filter(fun (a,b) -> a= "AA") |> List.map snd |> List.exactlyOne
let ends = labeledPoints |> List.filter(fun (a,b) -> a = "ZZ") |> List.map snd |> List.exactlyOne

let portals = 
    labeledPoints
    |> List.groupBy fst
    |> List.filter(fun s -> fst s <> "AA" && fst s <> "ZZ")
    |> List.map snd
    |> List.collect(fun p -> [snd p.Head, snd p.Tail.Head; snd p.Tail.Head, snd p.Head])
    |> Map

let neighbour (x,y) = 
    [
        x-1, y
        x+1, y
        x, y - 1
        x, y + 1
    ]
    |> List.filter(fun (a, b)-> cells.[b].[a] = ".")
    |> List.append( if portals.ContainsKey (x,y) then [portals.[(x,y)]] else [])
    |> Set

let portalNames = 
    labeledPoints
    |> List.map(fun (a,b) -> (b,a))
    |> Map

let bfs ((frontier, seen, n): Set<int*int>*Set<int*int>*int) =
    if frontier.Contains ends then 
        None
    else
        let newFrontier =
            frontier 
            |> Seq.map(fun u -> Set.difference (neighbour u) seen)
            |> Set.unionMany
        let newSeen = 
            Set.union seen frontier
        let newState = newFrontier, newSeen, n+1
        Some( n, newState)

let part1 = 
    Seq.unfold bfs ([start] |> Set, Set.empty, 1) |> Seq.last

type vertex = 
    { pos : int*int; level:int}

let start2 = {pos = start; level = 0}
let ends2 = {pos = ends; level = 0}

let outerPortals = 
    labeledPoints
    |> List.filter(fun (a,b) -> a <> "AA" && a<>"ZZ")
    |> List.map snd
    |> List.filter(fun (x,y) -> y <5 || y > 100 || x < 10 || x > 120)
    |> Set

let innerPortals = 
    labeledPoints
    |> List.filter(fun (a,b) -> a <> "AA" && a<>"ZZ")
    |> List.map snd
    |> List.filter(fun (x,y) -> not (y <5 || y > 100 || x < 10 || x > 120))
    |> Set

let neighbour2 (u:vertex) = 
    [
        { u with pos = (fst u.pos + 1, snd u.pos)}
        { u with pos = (fst u.pos - 1, snd u.pos)}
        { u with pos = (fst u.pos, snd u.pos - 1)}
        { u with pos = (fst u.pos, snd u.pos + 1)}
    ]
    |> List.filter(fun v-> cells.[snd v.pos].[fst v.pos] = ".")
    |> List.append( 
        if outerPortals.Contains u.pos && u.level > 0 then 
            [{pos = portals.[u.pos]; level = u.level - 1 }]
        else if innerPortals.Contains u.pos && u.level < 25 then
            [{pos = portals.[u.pos]; level = u.level + 1 }]
        else 
            [])
    |> Set

let bfs2 ((frontier, seen, n): Set<vertex>*Set<vertex>*int) =
    if frontier.Contains ends2 then 
        None
    else
        let newFrontier =
            frontier 
            |> Seq.map(fun u -> Set.difference (neighbour2 u) seen)
            |> Set.unionMany
            
        let newSeen = 
            Set.union seen frontier
        let newState = newFrontier, newSeen, n+1
        Some( n, newState)

let part2 = 
    Seq.unfold bfs2 ([start2]|>Set, Set.empty, 1) |> Seq.last