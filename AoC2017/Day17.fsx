let input = 380

let step inc buffer length =
    let position = inc % length + 1
    let a,b = buffer |> List.splitAt position
    [length] @ b @ a

let question1 = 
    let finalBuffer = [1..2017] |> List.fold (step input) [0] 
    finalBuffer.Item 1

type state = {Position: int; ValueNextToZero : int}

let step2 inc state length  = 
    let position = ((state.Position + inc) % length) + 1
    let valueNextToZero = match position with
                          | 1 -> length
                          | _ -> state.ValueNextToZero
    {Position = position; ValueNextToZero = valueNextToZero}

let question2 = 
    let state = [1..50000000] |> List.fold (step2 input) {Position = 0; ValueNextToZero = 0}
    state.ValueNextToZero