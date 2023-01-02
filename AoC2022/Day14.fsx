let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day14.txt")

let parseLine (line:string) = 
    line.Replace(" -> ",";").Split(';')
    |> Array.map(fun s -> s.Split(',') |> Array.map int)
    |> Array.windowed 2

let rock = 
    input
    |> Array.collect parseLine
    |> Array.map(fun x ->
        let a = x.[0]
        let b = x.[1]
        let lx = b.[0] - a.[0] |> abs
        let ly = b.[1] - a.[1] |> abs
        let dx,dy = 
            if lx = 0 then 
                0,(b.[1]-a.[1])/ly
            else if ly = 0 then
                (b.[0]-a.[0])/lx,0
            else failwith "unexpected line"
        Seq.init (lx + ly + 1) (fun i -> a.[0] + i * dx, a.[1] + i * dy) |> Set)
    |> Set.unionMany

let lowestRock = 
    rock
    |> Seq.map snd
    |> Seq.max

let sandDrop (stop: Set<int*int>*((int*int) option) -> bool)  ((rocks, sand): Set<int*int>*((int*int) option)) =
    if stop (rocks, sand) then None else
    if sand.IsNone then 
        let q = rocks, Some(500,0)
        Some(q,q)
    else
        let x = sand.Value |> fst
        let y = sand.Value |> snd
        let newSand = 
            if rocks.Contains(x,y+1) |> not then
                (x,y+1) |> Some
            else if rocks.Contains(x-1,y+1) |> not then
                (x-1, y+1) |> Some
            else if rocks.Contains(x+1,y+1) |> not then
                (x+1, y+1) |> Some
            else
                None
        if newSand.IsSome then 
            let q = rocks, newSand
            Some(q,q)
        else
            let q = rocks.Add(sand.Value), None
            Some(q,q)

let infinite ((_, sand): Set<int*int>*((int*int) option)) =
    sand 
    |> Option.map(fun s -> s|> snd > lowestRock)
    |> Option.defaultValue false

let congestion ((rocks, _): Set<int*int>*((int*int) option)) =
    rocks.Contains (500,0)

let fillCave stop initialRock= 
    let filledCave,_ = 
        Seq.unfold (sandDrop stop) (initialRock, None) |> Seq.last
    filledCave.Count - initialRock.Count


let floor = 
    let width = 1000
    Seq.init (2*width) (fun i -> -width + i, lowestRock + 2) |> Set

let part1 = fillCave infinite rock
let part2 = fillCave congestion (rock + floor)