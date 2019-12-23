let input = System.IO.File.ReadAllLines(@"D:\Src\Aoc2015\Input\Day18.txt") |> Array.map(fun s -> s.ToCharArray())

let getNeighbours i j (board : char array array)=
    let rows = [(max 0 (i-1))..(min (board.Length - 1) (i+1))]
    let cols = [(max 0 (j-1))..(min (board.Length - 1) (j+1))]
    let coordinates = rows |> List.collect(fun x -> cols |> List.map(fun y -> (x,y))) |> List.filter((<>) (i,j))
    coordinates |> List.filter(fun (x,y) -> board.[x].[y] = '#') |> List.length

let animatePixel i j c board =
    let cnt = getNeighbours i j board
    match c with
    | '.' -> 
        match cnt with
        | 3 -> '#'
        | _ -> '.'
    | '#' -> 
        match cnt with
        | 2 | 3 -> '#'
        | _ -> '.'
    | _ -> failwith "uventet char"

let animatePixel2 i j c (board : char array array)=
    let bl = board.Length - 1
    if i = 0 && j = bl then
        '#'
    else if i = 0 && j = 0 then
        '#'
    else if i = bl && j = 0 then
        '#'
    else if i = bl && j = bl then
        '#'
    else
        animatePixel i j c board

let animateRow (i : int) func (board : char array array) =
    let row = board.[i]
    row |> Array.mapi(fun j c -> func i j c board)

let step func board i=
    board |> Array.mapi(fun i r -> animateRow i func board)

let step1 board i= 
    step animatePixel board i
let step2 board i=
    step animatePixel2 board i

let question1 =
    [1..100] |> List.fold step1 input |> Array.concat |> Array.filter(fun c -> c = '#') |> Array.length

let question2 = 
    let row0 = input.[0] |> Array.mapi(fun i c -> match i with |0 | 99 -> '#' |_ -> c)
    let row99 = input.[99] |> Array.mapi(fun i c -> match i with |0 | 99 -> '#' |_ -> c)
    let newBoard = input |> Array.mapi(fun i r -> match i with |0 -> row0 |99 -> row99 |_ -> r)
    [1..100] |> List.fold step2 newBoard |> Array.concat |> Array.filter(fun c -> c = '#') |> Array.length