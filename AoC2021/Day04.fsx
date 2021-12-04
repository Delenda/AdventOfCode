let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day04.txt")

let drawnNumbers =
    input.[0].Split(',') |> Array.map int

let parseBoard (board: string array) =
    board 
    |> Array.map(fun s -> s.Split(' ') |> Array.filter(fun t -> t <> ""))
    |> Array.map(Array.map int)

let boards = 
    input
    |> Array.skip 1
    |> Array.chunkBySize 6
    |> Array.map Array.tail
    |> Array.map parseBoard

let makeBoardState (board: int array array) =
    let rows = board |> Array.map Set |> Array.toList
    let cols = board |> Array.mapi(fun i x -> x |> Array.mapi(fun j y -> board.[j].[i])) |> Array.map Set |> Array.toList
    rows@cols

let initialBoards = boards |> Array.map makeBoardState

let playSequence = 
    drawnNumbers 
    |> Array.scan(fun (_,boardStates) number -> 
        number,
        boardStates
        |> Array.filter(List.exists(Set.isEmpty) >> not)
        |> Array.map(List.map(Set.remove number))) (-1,initialBoards)

let boardScore pick= 
    let number,boardStates = 
        playSequence
        |> Seq.filter(fun (_,boardStates) -> boardStates |> Array.exists(List.exists(Set.isEmpty)))
        |> pick
    boardStates 
    |> Array.filter(List.exists(Set.isEmpty)) 
    |> Array.exactlyOne
    |> Set.unionMany  
    |> Seq.reduce (+) 
    |> (*) number

let part1  = boardScore Seq.head
let part2  = boardScore Seq.last   