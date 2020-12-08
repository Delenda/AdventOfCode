let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day08.txt")

type instruction =
    | Nop of int
    | Jmp of int
    | Acc of int

let instructions =
    let pattern = @"(\w{3})\s?([+|-]\d+)?\r\n"
    seq{for m in System.Text.RegularExpressions.Regex.Matches(input, pattern) do
            let count = m.Groups.[2].Value |> int
            let instr =
                match m.Groups.[1].Value with
                | "nop" -> Nop count
                | "jmp" -> Jmp count
                | "acc" -> Acc count
                | _ -> failwith ("Unexpected instruction: " + m.Groups.[1].Value)
            yield instr
        }
    |> Seq.toArray

type state =
    {
        SeenInstructions : Set<int>
        Pointer : int
        Accumulator : int
    }

let run (code: instruction array) =
    Seq.unfold(fun state ->
        if state.Pointer = code.Length then None else
        let newPointer, newAcc =
            match code.[state.Pointer] with
            | Nop j -> state.Pointer + 1, state.Accumulator
            | Jmp j -> state.Pointer + j, state.Accumulator
            | Acc j -> state.Pointer + 1, state.Accumulator + j
        if state.SeenInstructions.Contains newPointer then None else
        let newState = {Pointer = newPointer; Accumulator = newAcc; SeenInstructions = state.SeenInstructions.Add newPointer}
        Some (newState, newState)
    ) {Pointer = 0; Accumulator = 0; SeenInstructions = Set.empty}
    |> Seq.last

let part1 =
    run instructions
    |> fun s -> s.Accumulator

let part2 =
    let flip = function
        | Nop j -> Jmp j
        | Jmp j -> Nop j
        | Acc j -> Acc j

    [0..instructions.Length-1]
    |> List.map(fun i ->
        instructions
        |> Array.mapi(fun j op -> if j = i then flip op else op)
        |> run)
    |> List.filter(fun s -> s.Pointer = instructions.Length)
    |> List.exactlyOne
    |> fun s -> s.Accumulator