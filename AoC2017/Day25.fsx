type TuringState = | A | B | C | D | E | F

type state = {pos : int ; state : TuringState; tape : Map<int,bool>}

let step state _ =
    let currentValue = 
        let a  = state.tape.TryFind state.pos
        match a with
        | Some b -> b
        | _ -> false
    let newValue = 
        match currentValue with
        | true -> 
            match state.state with
            | E | F -> true
            | _ -> false
        | false -> true
    let newTuringState =
        match currentValue with
        | true -> 
            match state.state with
            | A -> E
            | B -> A
            | C -> C
            | D -> F
            | E -> C
            | F -> A
        | false -> 
            match state.state with
            | A -> B
            | B -> C
            | C -> D
            | D -> E
            | E -> A
            | F-> E
    let newPos = 
        match currentValue with
        | true -> 
            match state.state with
            | A -> state.pos - 1
            | B -> state.pos + 1
            | C -> state.pos + 1
            | D -> state.pos - 1
            | E -> state.pos - 1
            | F -> state.pos + 1
        | false -> 
            match state.state with
            | A -> state.pos + 1
            | B -> state.pos - 1
            | C -> state.pos - 1
            | D -> state.pos - 1
            | E -> state.pos - 1
            | F -> state.pos - 1
    let newTape = state.tape.Add(state.pos, newValue)
    {pos = newPos; state = newTuringState; tape = newTape}

let question1 = 
    let r = [1..12208951] |> List.fold step {pos = 0; state = A; tape = Map.empty}
    r.tape |> Map.filter(fun k t -> t) |> Seq.length