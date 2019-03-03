let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day19.txt")

type state = {i : int; j : int; letters : char list; Direction : int*int; Count : int}

let init = {i = 0; j= input.[0].IndexOf('|'); letters =  []; Direction = (1,0); Count = 1}

let letters = input |> Array.collect(fun s -> s.ToCharArray()) |> Array.filter(fun c -> c <> ' ' && c <> '-' && c<> '|' && c<>'+')

let getNewDirection newNode oldDirection =
        [(0,1); (0,-1); (1,0); (-1,0)] 
        |> List.filter( fun z ->  z <> oldDirection && z <> (- fst oldDirection, - snd oldDirection)) 
        |> List.map(fun (x,y) -> (x + fst newNode, y + snd newNode)) 
        |> List.filter(fun (x,y) -> x < input.Length)
        |> List.filter(fun (x,y) -> y < input.[x].Length)
        |> List.filter(fun (x,y) ->  input.[x].[y] = '|' || input.[x].[y] = '-' )
        |> List.map(fun (x,y) -> (x - fst newNode, y - snd newNode)) 
        |> List.exactlyOne
    
let step state =
    match state.letters.Length = letters.Length with
    | true -> None
    | _ ->
        let nx, ny = state.i + (fst state.Direction), state.j + (snd state.Direction)
        let c = input.[nx].[ny]
        let newState = 
            match c with
            | '-' | '|' ->  
                {i = nx; j = ny; Direction = state.Direction; letters = state.letters; Count = state.Count + 1}
            | '+' -> 
                let newDirection = getNewDirection (nx,ny) state.Direction
                {i = nx; j = ny; Direction = newDirection; letters = state.letters; Count = state.Count + 1}
            | _ when (letters |> Array.contains c) -> 
                 {i = nx; j = ny; Direction = state.Direction; letters = state.letters @ [c]; Count = state.Count + 1}
            | _ -> failwithf "Unexpected %c" c
        Some(newState,newState)

let question1 = 
    (Seq.unfold step init |> Seq.last).letters |> Array.ofList |> fun s -> System.String.Join("", s)

let question2 = 
    (Seq.unfold step init |> Seq.last).Count