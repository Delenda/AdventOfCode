type parameter = 
    | Position  of int64
    | Immediate of int64
    | Relative  of int64

type opcode = 
    | Halt 
    | Add                of parameter * parameter * parameter
    | Mult               of parameter * parameter * parameter
    | Read               of parameter
    | Write              of parameter
    | JumpNotNull        of parameter * parameter
    | JumpNull           of parameter * parameter
    | CompareLess        of parameter * parameter * parameter
    | CompareEqual       of parameter * parameter * parameter
    | AdjustRelativeBase of parameter

type programState = int64 * Map<int64,int64> * int64 list * int64 list

let getValue v (memory:Map<int64,int64>) = 
    match v with
    | Position a -> memory.TryFind a |> Option.defaultValue 0L
    | Immediate a -> a
    | Relative a -> memory.TryFind (a+memory.[-1L]) |> Option.defaultValue 0L

let setValue dest (memory:Map<int64,int64>) v=
    match dest with
    | Position a -> memory.Add(a, v)
    | Relative a -> memory.Add(a+memory.[-1L], v)
    | Immediate _ -> failwith("mode of destination cannot be immediate")

let nextPosition opcode position memory = 
    match opcode with
    | Halt -> -1L
    | Add _ | Mult _ | CompareLess _ | CompareEqual _ -> position + 4L
    | Read  _ -> position + 2L
    | Write _ -> position + 2L
    | JumpNotNull (a,b) -> if getValue a memory <> 0L then getValue b memory else position + 3L
    | JumpNull    (a,b) -> if getValue a memory =  0L then getValue b memory else position + 3L
    | AdjustRelativeBase _ -> position + 2L

let mode = function
    | 0L -> Position
    | 1L -> Immediate
    | 2L -> Relative
    | _ as x -> failwith(sprintf "Invalid mode: %d" x)
    
let nextOpcode position (memory: Map<int64,int64>) = 
    let instruction = memory.[position]
    let opcode = instruction%100L
    let mode1 = (instruction/100L)%10L   |> mode
    let mode2 = (instruction/1000L)%10L  |> mode
    let mode3 = (instruction/10000L)%10L |> mode

    match opcode with
    | 1L -> Add                 (mode1 memory.[position+1L],mode2 memory.[position+2L],mode3 memory.[position+3L])
    | 2L -> Mult                (mode1 memory.[position+1L],mode2 memory.[position+2L],mode3 memory.[position+3L])
    | 3L -> Read                (mode1 memory.[position+1L])                                                   
    | 4L -> Write               (mode1 memory.[position+1L])                                                   
    | 5L -> JumpNotNull         (mode1 memory.[position+1L],mode2 memory.[position+2L])                        
    | 6L -> JumpNull            (mode1 memory.[position+1L],mode2 memory.[position+2L])                        
    | 7L -> CompareLess         (mode1 memory.[position+1L],mode2 memory.[position+2L],mode3 memory.[position+3L])
    | 8L -> CompareEqual        (mode1 memory.[position+1L],mode2 memory.[position+2L],mode3 memory.[position+3L])
    | 9L -> AdjustRelativeBase  (mode1 memory.[position+1L])
    | 99L -> Halt
    | _ as x -> failwith(sprintf "Bad opcode: %d" x)

let modifyMemory opcode given position (memory:Map<int64,int64>) = 
    match opcode with
    | Add (a,b,c)           -> (getValue a memory) + (getValue b memory) |> setValue c memory
    | Mult(a,b,c)           -> (getValue a memory) * (getValue b memory) |> setValue c memory
    | Read a                -> setValue a memory (given |> List.head)
    | CompareLess (a,b,c)   -> (if getValue a memory < getValue b memory then 1L else 0L) |> setValue c memory
    | CompareEqual(a,b,c)   -> (if getValue a memory = getValue b memory then 1L else 0L) |> setValue c memory
    | AdjustRelativeBase a  -> (getValue a memory) + (getValue (Position -1L) memory) |> setValue (Position -1L) memory
    | Halt | JumpNull _ | JumpNotNull _ | Write _-> memory

let modifyOutput opcode (memory:Map<int64,int64>) output=
    match opcode with 
    | Write a -> (getValue a memory)::output
    | _ -> output

let modifyInput opcode given = 
    match opcode with
    | Read _ -> given |> List.tail
    | _ -> given

let isRead = function   
| Read _ -> true
| _ -> false

let programStep (position, memory, output, given)= 
    let opcode = nextOpcode position memory
    if opcode = Halt then
        None
    else if isRead opcode && (given |> List.isEmpty) then
        None
    else
        let newMemory = modifyMemory opcode given position memory
        let newOutput = modifyOutput opcode memory output
        let newPosition = nextPosition opcode position memory
        let newGiven =  modifyInput opcode given
        let newState = (newPosition, newMemory, newOutput, newGiven)
        Some (newState, newState)

let createMemory (program:string) = 
    program.Split(',') 
    |> Array.map int64 
    |> Array.indexed 
    |> Array.map(fun (i,c) -> int64 i , c)
    |> Map

let runProgram(program:string) (given:int64 list) = 
    let memory = createMemory program
    let output = []
    let startPosition = 0L
    let relativePosition = 0L

    let (_,_,outp,_)= 
        Seq.unfold programStep (startPosition, memory.Add(-1L,relativePosition), output, given)
        |> Seq.last
    outp