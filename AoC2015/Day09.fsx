let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day09.txt")

type edge = {source : string; target : string; distance :int}

let parse (str:string) =
    let s = str.Replace(" to ",";").Replace(" = ",";").Split(';')
    { source = s.[0]; target = s.[1]; distance = s.[2] |> int}

let flip e = {source = e.target; target = e.source; distance = e.distance }
let distances = (input |> Array.map parse |> Array.toList) @ (input |> Array.map (parse >> flip) |> Array.toList) |> Array.ofList 

let rec pathLength node dists maal=
    match dists |> Array.length with
    | 0 -> 0
    | _ -> dists 
            |> Array.filter(fun e -> e.source = node) 
            |> Array.map(fun e -> e.distance +  (pathLength e.target (dists |> Array.filter(fun e -> e.target <> node && e.source <> node)) maal) ) 
            |> maal

let question maal = 
    let startingPoints = Set.union (distances |> Array.map(fun e -> e.target) |> Set.ofArray) (distances |> Array.map(fun e -> e.source) |> Set.ofArray)
    startingPoints |> Seq.map(fun node -> pathLength node distances maal) |> Array.ofSeq |> maal

let question1 = question Array.min
let question2 = question Array.max