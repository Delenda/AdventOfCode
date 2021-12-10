let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day10.txt")

let reduce (tokens:char list) token = 
    if tokens.IsEmpty then [token],0 else
    match tokens.Head, token with
    | '(',')'| '{','}' |'[',']' |'<','>' -> tokens.Tail,0
    | _, ')' -> [], 3
    | _, ']' -> [], 57
    | _, '}' -> [], 1197
    | _, '>' -> [], 25137
    | _ -> token::tokens,0

let parse (line:string) =
    line.ToCharArray()
    |> Array.scan(fun (tokens,syntaxScore) char -> reduce tokens char) ([],0)

let score_Syntax (line:string) = 
    let parsedLine = 
        parse line
        |> Array.skipWhile(fun (tokens,score) -> score = 0)
        |> Array.toList
    if parsedLine.IsEmpty then 0 else parsedLine.Head |> snd

let part1 = input |> Array.sumBy score_Syntax

let score_Autocomplete (tokens:char list) = 
    tokens 
    |> List.fold(fun a b ->
        let s = 
             match b with 
             | '(' -> 1UL
             | '[' -> 2UL
             | '{' -> 3UL
             | '<' -> 4UL
             | _ -> failwith "begging the question"
        5UL*a + s) 0UL

let part2 = 
    let scores = 
        input
        |> Array.filter(fun line -> score_Syntax line = 0)
        |> Array.map parse
        |> Array.map Array.last
        |> Array.map fst
        |> Array.map score_Autocomplete
    scores
    |> Array.sort
    |> Array.skip(scores.Length/2)
    |> Array.head