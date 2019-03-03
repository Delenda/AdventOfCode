open System.Collections.Generic
let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day08.txt")

//Syntax symbol definition
type BinaryOperator = | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual | NotEqual | Equal
type Increment      = | Increase | Decrease                
type Register       = | Name of string
type Command = 
    {
        Target      : Register
        Source      : Register
        Operator    : BinaryOperator
        Increment   : Increment
        TargetValue : int
        SourceValue : int
    }

//Parsing
let parseOperator s = 
    match s with
    | "<"   -> LessThan
    | "<="  -> LessThanOrEqual
    | ">"   -> GreaterThan
    | ">="  -> GreaterThanOrEqual
    | "!="  -> NotEqual
    | "=="  -> Equal
    | _     -> failwith ("unknown operator: " + s)
let parseIncrement s =
    match s with
    | "inc" -> Increase
    | "dec" -> Decrease
    | _ -> failwith("unknown increment: " + s)
let parseLine (line : string) =
    let values = line.Split(' ')
    {
        Target      = values.[0] |> Name
        Source      = values.[4] |> Name
        Operator    = values.[5] |> parseOperator
        Increment   = values.[1] |> parseIncrement
        SourceValue = values.[6] |> int
        TargetValue = values.[2] |> int
    }
let parse i = i |> Array.map parseLine

//Compilation
type registerOperation = Dictionary<Register,int> -> unit
let compileBinaryOperator (o: BinaryOperator) =
    match o with
    | LessThan          -> fun a b -> a < b
    | LessThanOrEqual   -> fun a b -> a <= b
    | GreaterThan       -> fun a b -> a > b
    | GreaterThanOrEqual-> fun a b -> a >= b
    | NotEqual          -> fun a b -> a <> b
    | Equal             -> fun a b -> a = b
let compileCommand (cmd : Command) =
    let operator = compileBinaryOperator cmd.Operator
    let value = cmd.TargetValue * (match cmd.Increment with | Increase -> 1 | Decrease -> -1)
    let f (registers : Dictionary<Register,int>) = 
        match operator registers.[cmd.Source] cmd.SourceValue with
        | false -> ()
        | true -> registers.[cmd.Target]<- registers.[cmd.Target] + value
    f
let compile lines : registerOperation array * Register array=
    let registers = lines |> Array.collect(fun s -> [|s.Target; s.Source|]) |> Array.sort |> Array.distinct
    let commands = lines |> Array.map compileCommand
    commands, registers

//Program execution
let createRegister (reg : Register array)=
    let r = new Dictionary<Register,int>()
    reg |> Seq.iter(fun x -> r.Add(x, 0))
    r
let run1 pgm regNames =
    let r = createRegister regNames
    pgm |> Array.iter(fun c -> c r)
    r.Values |> Seq.max
let run2 pgm regNames =
    let r = createRegister regNames
    pgm |> Array.map(fun c -> c r,( r.Values |> Seq.max) ) |> Array.map snd |> Array.max

//Solution
let instructions            = parse     input
let program, registernames  = compile   instructions
run1 program registernames //Question 1
run2 program registernames //Question 2