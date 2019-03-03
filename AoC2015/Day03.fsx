let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day03.txt") |> Array.head
type state= {pos: int*int; visits : (int*int) Set}

let step state direction = 
    let newPos =
        match direction with
        | '<' -> (fst state.pos, snd state.pos - 1)
        | '>' -> (fst state.pos, snd state.pos + 1)
        | 'v' -> (fst state.pos - 1, snd state.pos)
        | '^' -> (fst state.pos + 1, snd state.pos)
    {pos= newPos; visits = state.visits.Add newPos}

let question1 =
    let finalState = input.ToCharArray() |> Array.fold step {pos = (0,0); visits = [(0,0)] |> Set.ofList} 
    finalState.visits.Count

let question2 = 
    let directions1, directions2 = input.ToCharArray() |> Array.chunkBySize 2 |> Array.map(fun s -> (s.[0], s.[1])) |> Array.unzip
    let fs1 = directions1 |> Array.fold step {pos = (0,0); visits = [(0,0)] |> Set.ofList} 
    let fs2 = directions2 |> Array.fold step {pos = (0,0); visits = [(0,0)] |> Set.ofList} 
    Set.union fs1.visits fs2.visits |> Set.count