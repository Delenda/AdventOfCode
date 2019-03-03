let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day10.txt")
type state =
    {
        CurrentPosition : int
        SkipSize        : int
        string          : int list
    }

let twist state length =
    let (a,b) = List.splitAt state.CurrentPosition state.string
    let (c,d) = b@a |> List.splitAt length
    let (e,f) = (List.rev c @ d) |> List.splitAt (state.string.Length - state.CurrentPosition)
    {state with CurrentPosition = (state.CurrentPosition + length + state.SkipSize) % state.string.Length; SkipSize = state.SkipSize + 1; string = f@e}

let initialState = {CurrentPosition = 0; SkipSize = 0; string = [0..255]}

let question1 =
    let lengths = input |> Array.map(fun s -> s.Split(',')) |> Array.exactlyOne |> Array.map int    
    let res = lengths |> Array.fold twist initialState
    match res.string with
    | a::b::_ -> a*b
    | _ -> failwith("Unexpected format")

let question2 (str : string) = 
    let lengths = (str.ToCharArray() |> Array.map int |> List.ofArray) @ [17;31;73;47;23]
    let totalLengths = [1..64] |> List.collect(fun i -> lengths)
    let res = totalLengths |> List.fold twist initialState
    let sparseHash = res.string |> List.chunkBySize 16
    let denseHash = sparseHash |> List.map( List.reduce (^^^)) 
    let hex = denseHash |> List.map(fun i -> System.String.Format("{0:x2}",i)) |> List.reduce (+)
    hex

let inputStr = input |> Array.exactlyOne
question2 inputStr