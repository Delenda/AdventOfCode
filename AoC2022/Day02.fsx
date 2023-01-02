let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day02.txt") 

type hand = |Rock|Paper|Scissors

let parse1 s = 
    if s = "A" then Rock else
    if s = "B" then Paper else
    if s = "C" then Scissors else
    failwith "Uventet input"

let parse2 s = 
    if s = "X" then Rock else
    if s = "Y" then Paper else
    if s = "Z" then Scissors else
    failwith "Uventet input"

let parse3 a b = 
    match (a,b) with
    | Rock, "X" -> Scissors
    | Rock, "Y" -> Rock
    | Rock, "Z" -> Paper
    | Paper, "X" -> Rock
    | Paper, "Y" -> Paper
    | Paper, "Z" -> Scissors
    | Scissors, "X" -> Paper
    | Scissors, "Y" -> Scissors
    | Scissors, "Z" -> Rock
    | _ -> failwith "UVentet input"


let scorePlay other play = 
    match (other, play) with
    | Rock, Paper -> 6
    | Rock, Rock -> 3
    | Rock, Scissors -> 0
    | Paper, Rock -> 0
    | Paper, Paper -> 3
    | Paper, Scissors -> 6
    | Scissors, Rock -> 6
    | Scissors, Paper -> 0
    | Scissors, Scissors -> 3

let playValue = function
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let test =
    [|
     "A Y"
     "B X"
     "C Z" |]

let part1 = 
    input
    |> Array.map(fun s -> parse1 (s.Substring(0,1)), parse2 (s.Substring(2,1)))
    |> Array.map(fun (other,play) -> (scorePlay other play) + playValue play)
    |> Array.sum

let part2 = 
    input
    |> Array.map(fun s -> parse1 (s.Substring(0,1)), s.Substring(2,1))
    |> Array.map(fun (a,b) -> a, parse3 a b)
    |> Array.map(fun (other,play) -> (scorePlay other play) + playValue play)
    |> Array.sum
