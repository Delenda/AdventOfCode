let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day24.txt") 

let digits = input |> Array.chunkBySize (input.Length/14)

type monadParameters = {a:int64;b:int64;c:int64}

let parseMonadProgram (program:string array) =
    {a = System.Text.RegularExpressions.Regex.Match(program.[4],  "div z (-?\d+)").Groups.[1].Value |> int64
     b = System.Text.RegularExpressions.Regex.Match(program.[5],  "add x (-?\d+)").Groups.[1].Value |> int64
     c = System.Text.RegularExpressions.Regex.Match(program.[15], "add y (-?\d+)").Groups.[1].Value |> int64}

let monadParameters = digits |> Array.map parseMonadProgram

let monadPart (parameters:monadParameters) inputDigit z=
    let x = if z%26L + parameters.b <> inputDigit then 1L else 0L
    (z/parameters.a)*(25L*x+1L)+(inputDigit+parameters.c)*x

let monad (model:int64) =
    let digits = [1..13] |> List.scan(fun s t -> s/10L) model |> List.map (fun d -> d%10L)  |> List.rev |> List.toArray
    [0..13] |> List.fold(fun z i -> monadPart monadParameters.[i] digits.[i] z) 0L

let upscale i z digit = monadPart monadParameters.[i] digit z
let downscale i z = monadPart monadParameters.[i] (monadParameters.[i].b + z%26L) z

let rec searchModel isAscending i z=
    if i = 14 then Some [] else
    if monadParameters.[i].a = 26L then
        let digit = (z%26L+monadParameters.[i].b)
        if digit < 1L || digit > 9L then None else
        let newZ = downscale i z
        searchModel isAscending (i+1) newZ |> Option.map(fun l -> digit::l)
    else
        let searchOrder = 
            if isAscending then
                [1L ..9L]
            else
                [9L .. -1L .. 1L]
        searchOrder
        |> Seq.fold(fun s digit -> 
            if s.IsSome then s else
            let newZ = upscale i z digit
            searchModel isAscending (i+1) newZ |> Option.map(fun l -> digit::l)) None
        
let part1 = searchModel false 0 0L |> Option.map(List.reduce(fun a b -> 10L*a + b))
let part2 = searchModel true  0 0L |> Option.map(List.reduce(fun a b -> 10L*a + b))