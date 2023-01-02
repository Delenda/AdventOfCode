let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day09.txt") 

let commands = 
    input
    |> Array.collect(fun s -> 
            let fields = s.Split(' ')
            let cnt = fields.[1] |> int
            Array.replicate cnt fields.[0])

type position = {X: int; Y:int}

let moveHead cmd pos = 
    match cmd with
    | "R" -> {pos with X = pos.X + 1}
    | "L" -> {pos with X = pos.X - 1}
    | "U" -> {pos with Y = pos.Y - 1}
    | "D" -> {pos with Y = pos.Y + 1}
    | _ -> failwith "Unknown command"

let moveTail head tail = 
    let dx = head.X - tail.X
    let dy = head.Y - tail.Y
    if abs dx < 2 && abs dy < 2 then 
        tail
    else
        let delta = {X = (if dx <> 0 then dx/(abs dx) else 0); Y = (if dy <> 0 then dy/(abs dy) else 0)}
        {X = tail.X + delta.X; Y = tail.Y + delta.Y}

let move tailLength = 
    commands
    |> Seq.scan(fun (h,rope) cmd -> 
        let newHead = moveHead cmd h
        let newRope = 
            rope
            |> List.scan moveTail newHead
            |> List.tail
        newHead, newRope) ({X=0;Y=0}, List.replicate tailLength {X=0;Y=0})
    |> Seq.map snd
    |> Seq.map List.rev
    |> Seq.map List.head
    |> Set
    |> Set.count

let part1 = move 1
let part2 = move 9