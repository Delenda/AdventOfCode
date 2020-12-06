let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt")

let answers = 
    input
        |> Array.fold(fun s t ->
            if t = "" then ((snd s)::fst s, "") else (fst s , (snd s) + " " + t)
            ) ([], "")
        |> fun (a,b) -> b::a

let count setfunction = 
    answers
    |> Seq.map(fun s -> 
        s.Trim().Split(' ') |> Array.map Set |> setfunction |> Set.count)
    |> Seq.sum

let part1 = count Set.unionMany
let part2 = count Set.intersectMany