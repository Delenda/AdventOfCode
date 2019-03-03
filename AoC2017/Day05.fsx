let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt")
let mutable jumps = input |> Array.map int

let solver incFunc=
    jumps <- input|> Array.map int
    let rec jump (position, n) =
        let increment = jumps.[position]
        jumps.[position] <- incFunc increment
        let newPosition = position + increment
        match newPosition < jumps.Length with
        | true -> jump (newPosition, n + 1)
        | false -> (newPosition, n + 1)
    jump (0, 0)

let question1Inc = fun x -> x + 1
let question2Inc = fun x -> match x > 2 with | true -> x - 1 | false -> x + 1

let question1 = solver question1Inc |> snd
let question2 = solver question2Inc |> snd