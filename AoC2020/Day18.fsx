let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day18.txt")

type symbol =
    | Digit of uint64
    | Plus
    | Mult
    | Left
    | Right

let lex (line:string) =
    line.Replace(" ", "").ToCharArray()
    |> Array.map(fun c ->
        match c with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> c |> string |> uint64 |> Digit
        | '+' -> Plus
        | '*' -> Mult
        | '(' -> Left
        | ')' -> Right
        | _ -> failwith (sprintf "unexpected input %s" (c.ToString())))
    |> Array.toList

type expression =
    | Const of uint64
    | Multiplication
    | Addition
    | SubExpression of expression list

type state =
    {
        Res: expression list
        Remain : symbol list
        Stop : bool
    }

let parse (line:string) =
    let rec innerParse (symbols:symbol list) =
        Seq.unfold(fun s ->
            if s.Remain.IsEmpty || s.Stop then None else
            let newState =
                match s.Remain.Head with
                | Digit a -> {Res = Const a::s.Res; Remain = s.Remain.Tail; Stop = false}
                | Mult -> {Res = Multiplication::s.Res; Remain = s.Remain.Tail; Stop = false}
                | Plus -> {Res = Addition::s.Res; Remain = s.Remain.Tail; Stop = false}
                | Left ->
                    let t = innerParse s.Remain.Tail
                    {Res = SubExpression t.Res::s.Res; Remain = t.Remain; Stop = false}
                | Right ->
                    {Res = s.Res; Remain = s.Remain.Tail; Stop = true}
            Some(newState,newState)
            ) {Res=[]; Remain = symbols; Stop = false}
        |> Seq.last
        |> fun s -> {s with Res = List.rev s.Res}
    (line |> lex |> innerParse).Res

let extractValueFromConst = function
    | Const x -> x
    |_  -> failwith "Error: not a const"

let rec reduce precedence_for_addition expr =
    let idx = expr |> List.tryFindIndex(fun t -> t = Addition)
    if precedence_for_addition && idx.IsSome then
        let reduced = expr |> List.skip (idx.Value - 1) |> List.take 3
        let a = [List.item 0 reduced] |> reduce true |> List.head |> extractValueFromConst
        let b = [List.item 2 reduced] |> reduce true |> List.head |> extractValueFromConst
        (List.take (idx.Value - 1) expr)@[Const(a+b)]@(List.skip (idx.Value + 2) expr) |> reduce true
    else
    match expr with
    | Const a::[] -> [Const a]
    | Const a::Multiplication::Const b::xs -> Const (a*b)::xs |> reduce precedence_for_addition
    | Const a::Addition::Const b::xs -> Const (a+b)::xs  |> reduce precedence_for_addition
    | Const a::Multiplication::SubExpression l::xs ->
        let tmp = reduce precedence_for_addition l
        let newExpr = if tmp.Length = 1 then tmp else [SubExpression tmp]
        Const a::Multiplication::tmp@xs |> reduce precedence_for_addition
    | Const a::Addition::SubExpression l::xs ->
        let tmp = reduce  precedence_for_addition l
        let newExpr = if tmp.Length = 1 then tmp else [SubExpression tmp]
        Const a::Addition::tmp@xs |> reduce precedence_for_addition
    | SubExpression l::xs ->
        let tmp = reduce precedence_for_addition l
        let newExpr = if tmp.Length = 1 then tmp else [SubExpression tmp]
        newExpr@xs |> reduce precedence_for_addition
    | _ -> failwith "Unexpected"

let doHomework addition_precedence =
    input
    |> Array.map parse
    |> Array.map (reduce addition_precedence)
    |> Array.collect(fun c -> [|Addition; c.Head|])
    |> Array.tail
    |> Array.toList
    |> reduce addition_precedence
    |> List.exactlyOne
    |> extractValueFromConst

let part1 = doHomework false
let part2 = doHomework true