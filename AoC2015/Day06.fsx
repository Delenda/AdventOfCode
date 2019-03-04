let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt")

type rectangle =
    { lux : int; luy :int; rlx : int ; rly : int}
    member this.Contains x = fst x >= this.lux && fst x <= this.rlx && snd x >= this.luy && snd x <= this.rly

type command = | On | Off | Toggle

type Instruction = {command : command; rectangle : rectangle}

let parseCommand (cmdStr : string) =
    match cmdStr with
    | "turn on" -> On
    | "turn off" -> Off
    | "toggle" -> Toggle
    | _ -> failwith (sprintf "Unable to parse command: %s" cmdStr)

let parse (str : string) =
    let pattern = "(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)"
    let regex = System.Text.RegularExpressions.Regex(pattern)
    let m = regex.Match(str)
    let intGroup (n:int) = m.Groups.[n].Value |> int
    if m.Success then
        let command = parseCommand (m.Groups.[1].Value)
        let rectangle = {lux = intGroup 2; luy = intGroup 3; rlx = intGroup 4; rly = intGroup 5}
        {command = command; rectangle = rectangle}
    else
        failwith (sprintf "Unable to parse '%s'" str)

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