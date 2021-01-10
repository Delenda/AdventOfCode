type register = A|B|C|D

type value = 
    | Register of register
    | Litteral of int64

type opcode = 
    | Cpy of value*value
    | Inc of value
    | Dec of value
    | Jnz of value*value
    | Tgl of value
    | Out of value

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
    | "cpy" -> Cpy ((parse_value terms.[1]),(parse_register terms.[2] |> Register))
    | "inc" -> Inc (parse_register terms.[1] |> Register)
    | "dec" -> Dec (parse_register terms.[1] |> Register)
    | "jnz" -> Jnz (parse_value terms.[1], parse_value terms.[2])
    | "tgl" -> Tgl (parse_value terms.[1])
    | "out" -> Out (parse_value terms.[1])
    | _ -> failwith ("Unexpected code: " + line)

type state = {Pointer: int64 ; Register :Map<register,int64>; Program : Map<int64, opcode>}

let eval (register:Map<register,int64>) v = 
    match v with
    | Litteral l -> l
    | Register r -> register.[r]

let toggle = function
    | Cpy(v,r) -> Jnz(v,r)
    | Inc r -> Dec r
    | Dec r -> Inc r
    | Jnz(v,offset) -> Cpy(v, offset)
    | Tgl v -> Inc v
    | Out v -> Inc v
    
let isRegister = function
    | Litteral _ -> false
    | Register _ -> true

let canExecute = function
    | Cpy(v,r) -> isRegister r
    | Inc r -> isRegister r
    | Dec r -> isRegister r
    | Jnz(v,offset) -> true
    | Tgl v -> true
    | Out v -> true

let register = function
    | Register x -> x
    | Litteral _ -> failwith("Value is not a register") 
    
let run (program:opcode array) (reg, regvalue) = 
    Seq.unfold(fun state ->
        if state.Pointer >= int64 state.Program.Count then None else
        let newRegister, offset, newProgram = 
            let opcode = state.Program.[state.Pointer]
            if canExecute opcode then
                match state.Program.[state.Pointer] with
                | Cpy (v, r) -> state.Register.Add(register r, eval state.Register v), 1L, state.Program
                | Inc r -> state.Register.Add(register r, state.Register.[register r] + 1L), 1L, state.Program
                | Dec r -> state.Register.Add(register r, state.Register.[register r] - 1L), 1L, state.Program
                | Jnz (v,offset) -> state.Register, (if eval state.Register v <> 0L then (eval state.Register offset) else 1L), state.Program
                | Tgl v -> 
                    let idx = state.Pointer + eval state.Register v
                    if idx < 16L then failwith ("Unexpected toggle")
                    if idx < 0L || idx >= int64 state.Program.Count then
                        state.Register, 1L, state.Program
                    else
                        state.Register, 1L, state.Program.Add(state.Pointer + eval state.Register v, toggle state.Program.[state.Pointer + eval state.Register v])
                | Out v -> 
                    System.Console.WriteLine (eval state.Register v)
                    state.Register, 1L, state.Program
            else
                state.Register, 1L, state.Program
                
        let newState = {Pointer = state.Pointer + offset; Register = newRegister; Program = newProgram}
        Some(newState.Register.[A], newState)
        ) {Pointer = 0L; Register = ([A,0L;B,0L;C,0L;D,0L] |> Map).Add(reg,regvalue); Program = program |> Seq.mapi(fun i c-> int64 i, c) |> Map}
    |> Seq.last
