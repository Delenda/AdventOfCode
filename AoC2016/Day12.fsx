let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day12.txt")

type register = A|B|C|D

type value = 
    | Register of register
    | Litteral of int64

type opcode = 
    | Cpy of value*register
    | Inc of register
    | Dec of register
    | Jnz of value*int

let parse_register (term:string) =
    match term with
    | "a" -> A
    | "b" -> B
    | "c" -> C
    | "d" -> D
    | _ -> failwith ("unexpected register")

let parse_value (term:string) =
    match term with
    | "a" -> Register A
    | "b" -> Register B
    | "c" -> Register C
    | "d" -> Register D
    | _ -> term |> int64 |> Litteral 

let parse (line:string) =
    let terms = line.Split(' ')
    match terms.[0] with
    | "cpy" -> Cpy ((parse_value terms.[1]),(parse_register terms.[2]))
    | "inc" -> Inc (parse_register terms.[1])
    | "dec" -> Dec (parse_register terms.[1])
    | "jnz" -> Jnz (parse_value terms.[1], int terms.[2])
    | _ -> failwith ("Unexpected code: " + line)

let program = input |> Array.map parse

type state = {Pointer: int ; Register :Map<register,int64>}

let eval (register:Map<register,int64>) v = 
    match v with
    | Litteral l -> l
    | Register r -> register.[r]

let run (program:opcode array) registerC= 
    Seq.unfold(fun state ->
        if state.Pointer >= program.Length then None else
        let newRegister, offset = 
            match program.[state.Pointer] with
            | Cpy (v, r) -> state.Register.Add(r, eval state.Register v), 1
            | Inc r -> state.Register.Add(r, state.Register.[r] + 1L), 1
            | Dec r -> state.Register.Add(r, state.Register.[r] - 1L), 1
            | Jnz (v,offset) -> state.Register, if eval state.Register v <> 0L then offset else 1
        let newState = {Pointer = state.Pointer + offset; Register = newRegister}
        Some(newState.Register.[A], newState)
        ) {Pointer = 0; Register = [A,0L;B,0L;C,registerC;D,0L] |> Map}
    |> Seq.last

let part1 = run program 0L
let part2 = run program 1L
