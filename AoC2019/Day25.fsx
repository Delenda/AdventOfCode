let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ +   @"\Input\Day25.txt") 

#load "IntCode.fsx"

let play (commands:string array) = 
    let intput=
        commands
        |> Array.map(fun s -> s.ToCharArray() |> Array.map int64 |> Array.toList)
        |> Array.map(fun l -> 10L::l)
        |> Array.toList
        |> List.collect id
    IntCode.runProgram input (intput@[10L])
    |>List.rev
    |>List.map (char>>string)
    |>String.concat ""

let commands d= 
    Array.concat
        [|
            [| "south";"east" |]
            (if d &&& 1 = 1 then [|"take space heater"|] else [||])
            [| "west";"south";"south";"east";"east"|]
            (if d &&& 2 = 2 then [|"take planetoid"|] else [||])
            [|"west";"west";"north";"north";"north";"east"|]
            (if d &&& 4 = 4 then [|"take spool of cat6"|] else [||])
            [|"north";"north"|]
            (if d &&& 8 = 8 then [|"take hypercube"|] else [||])
            [|"south";"south";"west";"north"|]
            (if d &&& 16 = 16 then [|"take festive hat"|] else [||])
            [|"west"|]
            (if d &&& 32 = 32 then [|"take dark matter"|] else [||])
            [|"north";"east"|]
            (if d &&& 64 = 64 then [|"take semiconductor"|] else [||])
            [|"east"|]
            (if d &&& 128 = 128 then [|"take sand"|] else [||])
            [|"north";"west"|]
        |], d

let commandId = 
    Seq.init 255 id
    |> Seq.toArray
    |> Array.map commands
    |> Array.Parallel.map (fun (a,b) -> b, play a)
    |> Seq.filter(fun (id,b) -> b.Contains("ejected") |> not)
    |> Seq.toList
    |> Seq.exactlyOne
    |> fst

let part1 = 
    commandId
    |> commands
    |> fst
    |> play
    |> System.Text.RegularExpressions.Regex("(\d+)").Matches
    |> fun m -> seq{for g in m do yield g.Groups.[1].Value |> int}
    |> Seq.last