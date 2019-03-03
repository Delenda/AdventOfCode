let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day13.txt")

let scanners = input |> Array.map(fun s -> s.Replace(": ",";")) |> Array.map(fun s -> s.Split(';')) |> Array.map(fun s -> (s.[0] |> int,s.[1] |> int))

let calculateSeverity offset delay (n,r) = 
    match (n + delay) % (2*r - 2) with
    | 0 -> n * r + offset // set offset > 0 to get severity = 0 ensures no detection
    | _ -> 0

let question1 =
    scanners |> Array.sumBy (calculateSeverity 0 0)

let question2 = 
    let wait i= 
        match scanners |> Array.sumBy(calculateSeverity 1 i) with
        | 0 -> None
        | _ -> Some(i, i + 1)
    Seq.unfold wait 0 |> Seq.length