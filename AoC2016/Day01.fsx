let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day01.txt") 

type position = 
    {x:int; y:int}
    member this.Distance = abs this.x + abs this.y
type direction = |North|East|South|West
type turn = Left | Right

let right = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North
let left = function  
    | North -> West
    | East -> North
    | South -> East
    | West -> South
let turn t d =
    match t with
    | Left -> left d
    | Right -> right d

let step d pos= 
    match d with 
    | North -> {pos with y = pos.y + 1}
    | West -> {pos with x = pos.x - 1}
    | South -> {pos with y = pos.y - 1}
    | East -> {pos with x = pos.x + 1}

let travel d pos steps = 
    Array.init steps id |> Array.scan(fun s t -> step d s) pos |> Array.tail

let pattern = "([L|R])(\d+)"
let regex = System.Text.RegularExpressions.Regex pattern
let path = 
    input.Split(',')
    |> Array.map(fun s -> regex.Match(s).Groups.[1].Value, regex.Match(s).Groups.[2].Value)
    |> Array.map(fun (a,b) -> (if a = "L" then Left else Right), int b)    
    |> Array.scan(fun (poss,direction) (t,steps) -> 
        let pos = poss |> Array.last
        let nd = turn t direction
        let newpos = travel nd pos steps
        newpos, nd
        ) ([|{x=0;y=0}|], North)
    |> Array.collect fst

let part1 = 
    let destination = path |> Array.last
    destination.Distance

let part2 = 
    let seen = Set.empty
    let firstRevisit = 
        path
        |> Array.scan(fun ((s,k) : Set<position> * position option) t -> 
            if s.Contains t then 
                s, Some t
            else 
                s.Add t,None
            ) (seen, None)
        |> Array.map snd
        |> Array.pick id
    firstRevisit.Distance

