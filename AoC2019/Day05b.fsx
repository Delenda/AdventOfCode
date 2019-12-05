let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt") 

type parameter = 
    | Position of int
    | Immediate of int

type opcode = 
    | Halt 
    | Add of parameter*parameter*parameter
    | Mult of parameter*parameter*parameter
    | Read of parameter
    | Write of parameter
    | JumpNotNull of parameter * parameter
    | JumpNull of parameter * parameter
    | CompareLess of parameter*parameter*parameter
    | CompareEqual of parameter*parameter*parameter

let getValue v (memory:Map<int,int>) = 
    match v with
    | Position a -> memory.[a]
    | Immediate a -> a

let setValue dest (memory:Map<int,int>) v=
    match dest with
    | Position a -> memory.Add(a, v)
    | Immediate _ -> failwith("mode of destination must be position")

let nextPosition opcode position memory = 
    match opcode with
    | Halt -> -1
    | Add _ | Mult _ | CompareLess _ | CompareEqual _ -> position + 4
    | Read  _ -> position + 2
    | Write _ -> position + 2
    | JumpNotNull (a,b) -> if getValue a memory <> 0 then getValue b memory else position + 3
    | JumpNull    (a,b) -> if getValue a memory =  0 then getValue b memory else position + 3

let mode a = if a = 0 then Position else Immediate
    
let nextOpcode position (memory: Map<int,int>) = 
    let instruction = memory.[position]
    let opcode = instruction%100
    let mode1 = (instruction/100)%10   |> mode
    let mode2 = (instruction/1000)%10  |> mode
    let mode3 = (instruction/10000)%10 |> mode

    match opcode with
    | 1 -> Add          (mode1 memory.[position+1],mode2 memory.[position+2],mode3 memory.[position+3])
    | 2 -> Mult         (mode1 memory.[position+1],mode2 memory.[position+2],mode3 memory.[position+3])
    | 3 -> Read         (mode1 memory.[position+1])
    | 4 -> Write        (mode1 memory.[position+1])
    | 5 -> JumpNotNull  (mode1 memory.[position+1],mode2 memory.[position+2])
    | 6 -> JumpNull     (mode1 memory.[position+1],mode2 memory.[position+2])
    | 7 -> CompareLess  (mode1 memory.[position+1],mode2 memory.[position+2],mode3 memory.[position+3])
    | 8 -> CompareEqual (mode1 memory.[position+1],mode2 memory.[position+2],mode3 memory.[position+3])
    | 99 -> Halt
    | _ as x -> failwith(sprintf "Bad opcode: %d" x)

let modifyMemory opcode given position (memory:Map<int,int>) = 
    match opcode with
    | Add (a,b,c)           -> (getValue a memory) + (getValue b memory) |> setValue c memory
    | Mult(a,b,c)           -> (getValue a memory) * (getValue b memory) |> setValue c memory
    | Read a                -> setValue a memory given
    | CompareLess (a,b,c)   -> (if getValue a memory < getValue b memory then 1 else 0) |> setValue c memory
    | CompareEqual(a,b,c)   -> (if getValue a memory = getValue b memory then 1 else 0) |> setValue c memory
    | Halt | JumpNull _ | JumpNotNull _ | Write _-> memory

let modifyOutput opcode (memory:Map<int,int>) output=
    match opcode with 
    | Write a -> (getValue a memory)::output
    | _ -> output

let programStep (position, memory, output, given) = 
    let opcode = nextOpcode position memory
    if opcode = Halt then
        None
    else
        let newMemory = modifyMemory opcode given position memory
        let newOutput = modifyOutput opcode memory output
        let newPosition = nextPosition opcode position memory
        Some (newOutput, (newPosition, newMemory, newOutput, given))

let runProgram (program:string) (given:int)=
    let memory = 
        program.Split(',') 
        |> Array.map int
        |> Array.indexed
        |> Map

    let output = []
    let startPosition = 0

    Seq.unfold programStep (startPosition, memory, output, given)
    |> Seq.last
    |> List.head

let part1 = runProgram input 1
let part2 = runProgram input 5