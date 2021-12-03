let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day02.txt")

type instruction = 
    | Forward of int
    | Down of int
    | Up of int


let parse (instruction:string) = 
    let fields = instruction.Split(' ') |> Seq.toList
    match fields with
    | "forward"::steps::_ -> steps |> int |> Forward
    | "down"::steps::_ -> steps |> int |> Down
    | "up"::steps::_ -> steps |> int |> Up
    | _ -> instruction |> sprintf "Unexpected instruction '%s'" |> failwith

let instructions = input |> Seq.map parse

type position = {Horizontal : int; Depth : int; Aim: int}
let initialPosition = {Horizontal = 0; Depth = 0; Aim = 0}



let manuever move = 
    instructions
    |> Seq.fold move initialPosition
    |> fun p -> p.Horizontal * p.Depth


let part1 =
    let move position instruction =
        match instruction with
        | Forward steps -> {position with Horizontal = position.Horizontal + steps}
        | Down steps -> {position with Depth = position.Depth + steps}
        | Up steps -> {position with Depth = position.Depth - steps}
    manuever move

let part2 = 
    let move position instruction =
        match instruction with
        | Forward steps -> {position with Horizontal = position.Horizontal + steps; Depth = position.Depth + position.Aim*steps}
        | Down steps -> {position with Aim = position.Aim + steps}
        | Up steps -> {position with Aim = position.Aim - steps}
    manuever move
