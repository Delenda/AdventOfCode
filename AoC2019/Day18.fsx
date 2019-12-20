let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +   @"\Input\Day18.txt")
let input2 = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +   @"\Input\Day18b.txt")

let cells = input |> Array.map(fun s -> s.ToCharArray())


type position = {x:int;y:int}


let letterPosition dots = 
    dots |> Array.mapi (fun i row -> 
        row |> Array.mapi (fun j c -> 
            if c < 'a' || c > 'z' then None else Some (c, {x= j; y = i})
            ))
    |> Array.collect id
    |> Array.choose id
    |> Map

let nbs n (dots:char array array) (seen:Set<position>) (openDoors :Set<char>) (pos:position) = 
    let ch = dots.[pos.y].[pos.x]
    if ('a' <= ch && ch <= 'z' && n > 0 && not(openDoors.Contains(char(int ch - 32)))) then Set.empty else
    [
        {x = pos.x + 1; y = pos.y}    
        {x = pos.x - 1; y = pos.y}    
        {x = pos.x;     y = pos.y + 1}
        {x = pos.x;     y = pos.y - 1}
    ]
    |> List.filter(fun p ->  
        let c = dots.[p.y].[p.x]
        c <> '#' && (c < 'A' || c > 'Z') || openDoors.Contains c) 
    |> List.filter(fun pos -> seen.Contains pos |> not)
    |> Set

let bfs (dots:char array array) ((frontier, seen, letters, openDoors,n): Set<position>*Set<position>*Map<char,int>*Set<char>*int) =
    if frontier.IsEmpty then 
        None
    else
        let newFrontier =
            frontier 
            |> Seq.map (nbs n dots seen openDoors )
            |> Set.unionMany
        let newSeen = 
            Set.union seen (frontier |> Set.map(fun p -> {x = p.x; y = p.y}))
        let foundLetters =
            newFrontier
            |> Set.map(fun pos -> dots.[pos.y].[pos.x])
            |> Set.filter(fun ch -> ch <> '.' && ch <> '#' && (ch < 'A' || ch > 'Z'))
            
        let newLetters = Seq.fold(fun (s:Map<char,int>) t -> s.Add(t, n) ) letters foundLetters

        let newState = newFrontier, newSeen, newLetters, openDoors, n + 1
        Some( newLetters, newState)



let reachableKeys dots (pos:position) (openDoors:Set<char>)= 
   Seq.unfold (bfs dots)([pos] |> Set, Set.empty, Map.empty, openDoors, 0) |> Seq.last

type vertex = {pos:position; letter:char; openDoors : Set<char>}

let neighbourDistances dots (letterPosition:Map<char,position>) (u:vertex) =
    if u.openDoors.Count = letterPosition.Count then Map.empty else
    reachableKeys dots u.pos u.openDoors
    |> Map.filter(fun k t -> k<>u.letter) 
    |> Seq.map(fun kvp -> {pos = letterPosition.[kvp.Key]; letter = kvp.Key; openDoors = u.openDoors.Add(char ((int kvp.Key) - 32))}, kvp.Value + 1)
    |> Map

let dijkstra dots (letterPosition:Map<char,position>) ((candidates,smallestDistance, previousVertex) : Set<vertex>*Map<vertex,int>*Map<vertex,vertex> ) =
    if candidates.IsEmpty then
        None
    else
        let u = candidates |> Seq.minBy(fun v -> smallestDistance.[v]) 
        let reachableDistances = neighbourDistances dots letterPosition u |> Map.filter(fun k t -> u.openDoors.Contains(char ((int k.letter) - 32)) |> not)
        let distU = smallestDistance.[u]
            
        let update ((dist,prev) : Map<vertex,int>*Map<vertex,vertex>) (v:vertex)  =
            let alternateDistance = distU + reachableDistances.[v]
            if not (dist.ContainsKey v) then 
                dist.Add(v, alternateDistance), prev.Add(v,u)
            else if alternateDistance < dist.[v] then
                let di = dist.Remove v
                let pr = prev.Remove v
                di.Add(v, alternateDistance), pr.Add(v,u)
            else
                dist,prev
        let newVertices = reachableDistances |> Seq.map(fun kvp -> kvp.Key) |> Set
        let newDistance , newPrevious = newVertices  |> Seq.fold update (smallestDistance, previousVertex)
        let newCandidates = Set.union newVertices (candidates.Remove u)
        let q = newCandidates,newDistance,newPrevious
        Some(q,q)

let seek dots = 
    let entrance = dots |> Array.mapi(fun i row -> row |> Array.mapi(fun j c -> if c <> '@' then None else Some {pos = {x=j;y=i}; letter = '.'; openDoors = Set.empty})) |> Array.collect id |> Array.choose id |> Array.exactlyOne
    let newDots = dots |> Array.map(fun row -> row |> Array.map(fun c -> if c = '@' then '.' else c))
    let lp = letterPosition newDots
    let c,d,p = Seq.unfold (dijkstra newDots lp)([entrance] |> Set, [entrance,0] |> Map, Map.empty) |> Seq.last
    d |> Map.filter(fun k t -> k.openDoors.Count = lp.Count) |> Seq.map(fun kvp -> kvp.Value) |> Seq.min

let test1 = 
    [|  "#########"
        "#b.A.@.a#"
        "#########"|] |> Array.map(fun s -> s.ToCharArray())

seek test1

let test2 = 
    [|
        "########################"
        "#f.D.E.e.C.b.A.@.a.B.c.#"
        "######################.#"
        "#d.....................#"
        "########################"
    |]|> Array.map(fun s -> s.ToCharArray())

seek test2

let test3 = 
    [|
        "########################"
        "#...............b.C.D.f#"
        "#.######################"
        "#.....@.a.B.c.d.A.e.F.g#"
        "########################"
    |]|> Array.map(fun s -> s.ToCharArray())

seek test3

let test4 = 
    [|
        "#################"
        "#i.G..c...e..H.p#"
        "########.########"
        "#j.A..b...f..D.o#"
        "########@########"
        "#k.E..a...g..B.n#"
        "########.########"
        "#l.F..d...h..C.m#"
        "#################"
    |]|> Array.map(fun s -> s.ToCharArray())

seek test4

let test5 = 
    [|
        "########################"
        "#@..............ac.GI.b#"
        "###d#e#f################"
        "###A#B#C################"
        "###g#h#i################"
        "########################"
    |]|> Array.map(fun s -> s.ToCharArray())

seek test5

let part1 = seek cells

let subGraph1 = 
    input2 |> Array.map(fun s -> s.ToCharArray())
    |> Array.take 41
    |> Array.map(Array.take 41)

let subGraph2 = 
    input2 |> Array.map(fun s -> s.ToCharArray())
    |> Array.skip 40
    |> Array.map(Array.take 41)

let subGraph3 = 
    input2 |> Array.map(fun s -> s.ToCharArray())
    |> Array.take 41
    |> Array.map(Array.skip 40)

let subGraph4 = 
    input2 |> Array.map(fun s -> s.ToCharArray())
    |> Array.skip 40
    |> Array.map(Array.skip 40)

let seekSubGraph dots = 
    let newDots = dots |> Array.map(fun row -> row |> Array.map(fun c -> if c = '@' then '.' else c))
    let lp = letterPosition newDots
    let openDoors = Set ['a'..'z'] |> Set.filter(lp.ContainsKey >> not) |> Set.map(fun ch -> int ch - 32 |> char)
    let entrance = dots |> Array.mapi(fun i row -> row |> Array.mapi(fun j c -> if c <> '@' then None else Some {pos = {x=j;y=i}; letter = '.'; openDoors = openDoors})) |> Array.collect id |> Array.choose id |> Array.exactlyOne
    let c,d,p = Seq.unfold (dijkstra newDots lp)([entrance] |> Set, [entrance,0] |> Map, Map.empty) |> Seq.last
    d |> Map.filter(fun k t -> k.openDoors.Count = openDoors.Count + lp.Count) |> Seq.map(fun kvp -> kvp.Value) |> Seq.min

let part2 = 
    [subGraph1; subGraph2; subGraph3; subGraph4] |> List.sumBy seekSubGraph