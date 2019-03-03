let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt")

type rectangle = 
    { lux : int; luy :int; rlx : int ; rly : int}
    member this.Contains x = fst x >= this.lux && fst x <= this.rlx && snd x >= this.luy && snd x <= this.rly

type command = | On | Off | Toggle 

type Instruction = {command : command; rectangle : rectangle}

let parse (str : string)  =
    let s = str.Replace("turn on","On").Replace("turn off","Off").Split(' ')
    let command = 
        match s.[0] with
        | "On" -> On
        | "Off" -> Off
        | _ -> Toggle
    let lu = s.[1].Split(',')
    let rl = s.[3].Split(',')
    let rectangle = {lux = lu.[0] |> int; luy = lu.[1] |> int; rlx = rl.[0] |> int; rly = rl.[1] |> int}
    {command = command; rectangle = rectangle}

let instructions = input |> Array.map parse

let determine1 (a,b) tændt instruction=
    match instruction.rectangle.Contains (a,b) with
    | false -> tændt
    | true -> 
        match instruction.command with
        | On -> true
        | Off -> false
        | Toggle -> tændt |> not

let determine2 (a,b) tændt instruction=
    match instruction.rectangle.Contains (a,b) with
    | false -> tændt
    | true -> 
        match instruction.command with
        | On -> tændt + 1
        | Off -> max (tændt - 1) 0
        | Toggle -> tændt + 2

let pairs = [0..999] |> List.collect(fun i -> [0..999] |> List.map(fun j -> (i,j)))

let question1 = 
    pairs |> List.map(fun x -> instructions |> Array.fold (determine1 x) false) |> List.filter id |> List.length
let question2 = 
    pairs |> List.map(fun x -> instructions |> Array.fold (determine2 x) 0) |> List.sum