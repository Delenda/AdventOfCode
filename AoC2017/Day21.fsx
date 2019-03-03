let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day21.txt")

let expansions  = input |> Array.map(fun s -> s.Replace(" => ",";").Split(';')) |> Array.map(fun s -> (s.[0].Split('/'),s.[1].Split('/'))) |> Map.ofArray
let init = ".#./..#/###".Split('/')
let length (grid : string array) = grid.[0].Length
let width grid =
    match length grid % 2 with
    | 0 -> 2
    | _ -> 3

let getSubSquare (grid : string array) x y w =
    [|0..(w-1)|] |> Array.map(fun i -> grid.[i+x*w].Substring(y*w,w))

let r square = 
    match length square with
    | 2 -> [| 
                square.[0].Substring(1,1) + square.[1].Substring(1,1)
                square.[0].Substring(0,1) + square.[1].Substring(0,1)
           |]
    | _ -> [| 
                square.[0].Substring(2,1) + square.[1].Substring(2,1) + square.[2].Substring(2,1)
                square.[0].Substring(1,1) + square.[1].Substring(1,1) + square.[2].Substring(1,1)
                square.[0].Substring(0,1) + square.[1].Substring(0,1) + square.[2].Substring(0,1)
           |]

let f (square : string array) = 
    square |> Array.rev
    
let DihedralGroup =  [id; r; r>>r; r>>r>>r; f; f>>r; f>>r>>r; f>>r>>r>>r]

let expandSquare sq =
    let squareOrbit = DihedralGroup |> List.map(fun g -> g sq) |> List.distinct
    squareOrbit |> List.filter(fun k -> expansions |> Map.containsKey k) |> List.map(fun k -> expansions.[k]) |> List.exactlyOne

let doExpansion grid = 
    let w = width grid
    let l = length grid
    let subdivision = l / w
    let squares =  [|0..(subdivision-1)|] |> Array.map(fun x -> [|0..(subdivision-1)|] |> Array.map(fun y -> getSubSquare grid x y w |> expandSquare ))
    let newGrid = [|0..(subdivision-1)|] |> Array.collect(fun i -> [|0..w|] |> Array.map(fun k -> [|0..(subdivision-1)|] |> Array.map(fun j -> squares.[i].[j].[k]) |> String.concat ""))
    newGrid

let question n (grid: string array) =
    [1..n] |> List.fold (fun s t -> doExpansion s) grid |> Array.map(fun s -> s.Replace(".","").Length) |> Array.sum
    
let question1 = question 5
let question2 = question 18
    
question1 init
question2 init