let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day03.txt") 

type position = 
    {x:int; y:int}
    member this.Distance = abs this.x + abs this.y
type direction = |Up|Down|Left|Right

let step d pos= 
    match d with 
    | Up -> {pos with y = pos.y + 1}
    | Left -> {pos with x = pos.x - 1}
    | Down -> {pos with y = pos.y - 1}
    | Right -> {pos with x = pos.x + 1}

let travel d pos steps = 
    Array.init steps id |> Array.scan(fun s t -> step d s) pos |> Array.tail

let pattern = "([U|D|L|R])(\d+)"
let regex = System.Text.RegularExpressions.Regex pattern
let path (textpath:string)= 
    textpath.Split(',')
    |> Array.map(fun s -> regex.Match(s).Groups.[1].Value, regex.Match(s).Groups.[2].Value)
    |> Array.map(fun (a,b) -> (if a = "L" then Left else if a = "R" then Right else if a = "D" then Down else Up), int b)    
    |> Array.scan(fun poss (direction,steps) -> 
        let pos = poss |> Array.last
        let newpos = travel direction pos steps
        newpos
        ) [|{x=0;y=0}|]
    |> Array.collect id

let path1 = path input.[0] |> Array.tail
let path2 = path input.[1] |> Array.tail

let part1 = 
    let positions1 = path1 |> Set
    let positions2 = path2 |> Set
    Set.intersect positions1 positions2
    |> Seq.minBy(fun x -> x.Distance)
    |> fun x -> x.Distance

let part2 = 
    let steps (p: position array) = 
        p 
        |> Array.mapi(fun i x -> x,i+1)
        |> Array.groupBy fst
        |> Array.map(fun (loc, dists) -> (loc, dists |> Array.map snd |> Array.min ) )
        |> Map
    let steps1 = steps path1
    let steps2 = steps path2
    Set.intersect (steps1 |> Seq.map(fun k -> k.Key) |> Set) (steps2 |> Seq.map(fun k -> k.Key) |> Set)
    |> Seq.minBy(fun position -> steps1.[position] + steps2.[position])
    |> fun x -> steps1.[x] + steps2.[x]