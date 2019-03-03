let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day07.txt")

type ValOrWire = 
    | Wire of string
    | Value of int

type operation =    
    | Not of ValOrWire 
    | And of ValOrWire * ValOrWire
    | Or of ValOrWire * ValOrWire
    | Set of ValOrWire 
    | RShift of ValOrWire * ValOrWire
    | LShift of ValOrWire * ValOrWire

let create (str:string) = 
    match str.[0] |> int >= 97 with
    | true -> ValOrWire.Wire str
    | false -> str |> int |> ValOrWire.Value

let parse (str : string) = 
    printfn "%s" str
    let s = str.Split(' ')
    match s.Length with
    | 3 -> (s.[2], Set(s.[0] |> create))
    | 4 -> (s.[3], Not(s.[1] |> create))
    | 5 -> 
        match s.[1] with
        | "AND" ->  (s.[4], And(s.[0] |> create, s.[2] |> create)) 
        | "OR"  -> (s.[4], Or(s.[0] |> create, s.[2] |> create))
        | "LSHIFT" -> (s.[4], LShift(s.[0] |> create, s.[2] |> create))
        | "RSHIFT" ->(s.[4], RShift(s.[0] |> create, s.[2] |> create))
        | _ -> failwithf "unknown instruction : %s" s.[1]
    | _ -> failwith ("could not parse:" + str)

let connections = input |> Array.map parse |> Map.ofArray

//Didn't work. infinite loop
//let rec getValue (wov : ValOrWire) =
//    printfn "%A" wov
//    match wov with
//    | Value x -> x
//    | Wire wire ->
//        let op = combi.[wire]
//        match op with
//        | Not x -> ~~~(getValue x)
//        | And (x, y) -> (getValue x)&&&(getValue y)
//        | Or (x, y) -> (getValue x)^^^(getValue y)
//        | Set x -> getValue x
//        | RShift (x, y) -> (getValue x)>>>(getValue y)
//        | LShift (x, y) -> (getValue x)<<<(getValue y)
//
//getValue( Wire "a")

let getValue (values: Map<string,int>) vow = 
    match vow with
    | Value x -> x
    | Wire x -> values.[x]

let isAccessable (values : Map<string, int>) vow =
    match vow with
    | Value x -> true
    | Wire x -> values.ContainsKey x

let canBeEvaluated (values: Map<string,int>) vow =
    match vow with
    | Not x -> isAccessable values x
    | Set x -> isAccessable values x
    | And(x,y) -> (isAccessable values x) && (isAccessable values y)
    | Or(x,y) -> (isAccessable values x) && (isAccessable values y)
    | LShift(x,y) -> (isAccessable values x) && (isAccessable values y)
    | RShift(x,y) -> (isAccessable values x) && (isAccessable values y)

let eval (values: Map<string,int>) op =
    match op with
    | Not x -> ~~~(getValue values x)
    | Set x -> getValue values x
    | And(x,y) -> (getValue values x)&&&(getValue values y)
    | Or(x,y) -> (getValue values x)^^^(getValue values y)
    | LShift(x,y) -> (getValue values x)<<<(getValue values y)
    | RShift(x,y) -> (getValue values x)>>>(getValue values y)

type state=  {values: Map<string,int>; connections : Map<string, operation>}
let evaluate state =
    match state.connections.Count with
    | 0 -> None
    | _ -> 
        let evaluations = state.connections |> Map.filter(fun k t -> canBeEvaluated state.values t) |> Map.map(fun k t -> eval state.values t)
        let newValues = evaluations |> Map.fold (fun (s: Map<string,int>) k t -> s.Add(k, t)) state.values
        let newInstructions = evaluations |> Map.fold (fun (s : Map<string,operation>) k t -> s.Remove k) state.connections
        let newState = {values = newValues; connections = newInstructions}
        Some (newValues, newState)

let question1 = 
    let wires = Seq.unfold evaluate {values  = Map.empty; connections = connections} |> Seq.last
    wires.["a"]

let question2 = 
    let a = (Seq.unfold evaluate {values  = Map.empty; connections = connections} |> Seq.last).["a"]
    let init = {values = [("b", a)] |> Map.ofList; connections = connections.Remove "b"}
    let wires = Seq.unfold evaluate init|> Seq.last
    wires.["a"]    