let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day18.txt")

type symbol =
    | Digit of uint64
    | Plus
    | Asterisk
    | Open
    | Close

let lex (line:string) =
    line
        .ToCharArray()
        |> Array.filter(fun c -> c <> ' ')
        |> Array.map(fun c ->
            match c with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> c |> string |> uint64 |> Digit
            | '+' -> Plus
            | '*' -> Asterisk
            | '(' -> Open
            | ')' -> Close
            | _ -> failwith (sprintf "unexpected input '%s'" (c.ToString())))
        |> Array.toList

type expression =
    | Const of uint64
    | Multiplication
    | Addition
    | Expression of expression list

type state =
    {
        Result: expression list
        Stream : symbol list
        Stop : bool
    }

let parse (line:string) =
    let rec innerParse (symbols:symbol list) =
        Seq.unfold(fun s ->
            if s.Stream.IsEmpty || s.Stop then None else
            let newState =
                match s.Stream.Head with
                | Digit a   -> {Result = Const a::s.Result;         Stream = s.Stream.Tail; Stop = false}
                | Asterisk  -> {Result = Multiplication::s.Result;  Stream = s.Stream.Tail; Stop = false}
                | Plus      -> {Result = Addition::s.Result;        Stream = s.Stream.Tail; Stop = false}
                | Close     -> {Result = s.Result;                  Stream = s.Stream.Tail; Stop = true}
                | Open      ->
                    let t = innerParse s.Stream.Tail
                    {Result = Expression t.Result::s.Result; Stream = t.Stream; Stop = false}
            Some(newState,newState)
            ) {Result=[]; Stream = symbols; Stop = false}
        |> Seq.last
        |> fun s -> {s with Result = List.rev s.Result}
    (line |> lex |> innerParse).Result

let extractValueFromConst = function
    | Const x -> x
    |_  -> failwith "Error: not a const"

type precedence = | Left_to_right | Addition_has_precedence

let rec reduce precedence expr =
    let idx = expr |> List.tryFindIndex(fun t -> t = Addition)
    if precedence = Addition_has_precedence && idx.IsSome then
        let initialSegment, tail  = expr |> List.splitAt (idx.Value - 1)
        let addents, finalSegment = tail |> List.splitAt 3
        let sum =
            addents
            |> List.filter(fun s -> s <> Addition)
            |> List.map(fun s -> reduce precedence [s])
            |> List.collect (List.map extractValueFromConst)
            |> List.sum
        initialSegment@[Const sum]@finalSegment |> reduce precedence
    else
    let reduction =
        match expr with
        | Const a::[]                           -> [Const a]
        | Const a::Multiplication::Const b::xs  -> Const (a*b)::xs
        | Const a::Addition::Const b::xs        -> Const (a+b)::xs
        | Const a::b::Expression inner::xs      -> Const a::b::reduce precedence inner@xs
        | Expression inner::xs                  -> reduce precedence inner@xs
        | _ -> failwith "Unexpected"
    if reduction.Length = 1 then
        reduction
    else
        reduce precedence  reduction

let doHomework precedence =
    input
    |> Array.toList
    |> List.map parse
    |> List.collect (reduce precedence)
    |> List.sumBy extractValueFromConst

let part1 = doHomework Left_to_right
let part2 = doHomework Addition_has_precedence