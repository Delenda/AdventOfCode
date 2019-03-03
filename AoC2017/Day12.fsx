let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day12.txt")
let links = 
    input 
    |> Array.map(fun s -> s.Replace(" <-> ", ";"))
    |> Array.collect(fun s -> s.Split(';').[1].Split(',') |> Array.map(fun t -> (s.Split(';').[0] |> int, t |> int)))
    |> List.ofArray

let totalLinks = links @ (links |> List.map(fun (x,y) -> (y,x))) |> List.distinct

let step programs =
    let newPrograms =
        programs
        |> List.collect(fun p -> totalLinks |> List.filter(fun (x,y) -> x = p) |> List.map snd)
    let output = programs @ newPrograms |> List.distinct
    match output.Length > programs.Length with
    | true -> Some(output, output)
    | false -> None
    
let question1 = 
    let result = Seq.unfold step [0] |> List.ofSeq
    result |> List.last |> List.length

let question2 = 
    let oneCycles = totalLinks |> List.filter(fun (x,y) -> x = y) |> List.map fst |> List.filter(fun p -> totalLinks |> List.exists(fun (x,y) -> x<>y && x = p) |> not)
    let totalPrograms = totalLinks |> List.map fst |> List.filter(fun p -> oneCycles |> List.contains p |> not) |> List.distinct
    let step2 programsLeft =
        match programsLeft |> List.length with
        | 0 -> None
        |_ -> 
            let fstProgram = programsLeft.Head
            let group = Seq.unfold step [fstProgram] |> Seq.last
            let dif = programsLeft |> List.filter(fun  p -> group |> List.contains p |> not)
            Some (group, dif)
    (Seq.unfold step2 totalPrograms |> Seq.length) + (oneCycles |> List.length)