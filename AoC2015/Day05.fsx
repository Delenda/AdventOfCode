let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt")
let isVowel (c : char) = "aeiou".IndexOf(c) > -1

let question1 = 
    input
        |> Array.filter(fun s -> s.Contains("ab") |> not)
        |> Array.filter(fun s -> s.Contains("cd") |> not)
        |> Array.filter(fun s -> s.Contains("pq") |> not)
        |> Array.filter(fun s -> s.Contains("xy") |> not)
        |> Array.filter(fun s -> s |> Seq.filter isVowel |> Seq.length > 2)
        |> Array.filter(fun s -> s |> Seq.pairwise |> Seq.filter(fun (x,y) -> x = y) |> Seq.length > 0)
        |> Array.length

let question2 =
    input 
        |> Array.filter(fun s -> [0..(s.Length-3)] |> List.map(fun i -> s.[i], s.[i+2]) |> List.filter(fun (x,y) -> x = y) |> List.length > 0)
        |> Array.filter(fun s -> [0..(s.Length-4)] |> List.map(fun i -> i,s.Substring(i,2)) |> List.filter(fun (i,t) -> s.Substring(i+2).Contains(t)) |> List.length > 0)
        |> Array.length