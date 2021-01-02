let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day23.txt")

type register = A | B

type opcode = 
    | Hlf of register
    | Tpl of register
    | Inc of register
    | Jmp of int
    | Jie of register * int
    | Jio of register * int

let regFromString str = 
    match str with
    | "a" -> A
    | "b" -> B
    | _  -> failwith (sprintf "Unexpected string: %s" str)

type state = { Register : Map<register, uint64>; Pointer : int}

let parse (line:string) = 
    let pattern = @"((hlf) ([a|b])|(tpl) ([a|b])|(inc) ([a|b])|(jmp) (\d+)|(jie) ([a|b]), (\d+)|(jio) ([a|b]), (\d+))+"
    let terms = line.Replace(",","").Split(' ')
    match terms.[0] with
    | "tpl" -> Tpl (regFromString terms.[1])
    | "hlf" -> Hlf (regFromString terms.[1])
    | "inc" -> Inc (regFromString terms.[1])
    | "jmp" -> Jmp (int terms.[1])
    | "jie" -> Jie (regFromString terms.[1], int terms.[2])
    | "jio" -> Jio (regFromString terms.[1], int terms.[2])
    | _ -> failwith ("unexpected opcode: " + terms.[0])

let program = input |> Array.map parse

let runProgram initialA = 
    let initialState = {Pointer = 0; Register = Map [A,initialA;B,0UL]}
    Seq.unfold(fun state -> 
        if state.Pointer >= program.Length then None else
        let offset, newRegister = 
            match program.[state.Pointer] with
            | Hlf r     -> 1, state.Register.Add(r, state.Register.[r]/2UL)
            | Tpl r     -> 1, state.Register.Add(r, state.Register.[r]*3UL)
            | Inc r     -> 1, state.Register.Add(r, state.Register.[r] + 1UL)
            | Jmp offset -> offset, state.Register
            | Jie (r, offset) -> (if state.Register.[r]%2UL = 0UL then offset else 1), state.Register
            | Jio (r, offset) -> (if state.Register.[r] = 1UL then offset else 1), state.Register
        let newPointer = 
            if state.Pointer + offset < 0 then 
                state.Pointer + offset + program.Length
            else
                state.Pointer + offset
        let newState = {Pointer = newPointer; Register = newRegister}
        Some( newRegister.[B], newState)
        ) initialState
    |> Seq.last

let part1 = runProgram 0UL
let part2 = runProgram 1UL  