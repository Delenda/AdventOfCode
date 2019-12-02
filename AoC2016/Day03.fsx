let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day03.txt") 

let pattern = "(\d+)\s+(\d+)\s+(\d+)"
let regex = System.Text.RegularExpressions.Regex pattern
let part1 = 
    input
    |> Seq.map(fun s -> regex.Match(s).Groups)
    |> Seq.map(fun gr -> int (gr.[1].Value), int (gr.[2].Value), int (gr.[3].Value))
    |> Seq.filter(fun (a,b,c) -> a + b > c && a + c > b && b + c > a)
    |> Seq.length

let part2 = 
    input
    |> Seq.map(fun s -> regex.Match(s).Groups)
    |> Seq.map(fun gr -> int (gr.[1].Value), int (gr.[2].Value), int (gr.[3].Value))
    |> Seq.map(fun (a,b,c) -> [|a;b;c|])
    |> Seq.chunkBySize 3
    |> Seq.map Seq.toArray
    |> Seq.collect(fun a -> [|a.[0].[0], a.[1].[0], a.[2].[0];a.[0].[1], a.[1].[1], a.[2].[1];a.[0].[2], a.[1].[2], a.[2].[2]|])
    |> Seq.filter(fun (a,b,c) -> a + b > c && a + c > b && b + c > a)
    |> Seq.length