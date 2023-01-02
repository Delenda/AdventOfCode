let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day03.txt") 

let priority (c:char) =
    let v = c |> int
    let aInt = 'a' |> int
    let AInt = 'A' |> int
    if v >= aInt then 
       v - aInt + 1
    else
        v - AInt + 27

let part1 = 
    input
    |> Array.map(fun s -> s.Substring(0,s.Length/2), s.Substring(s.Length/2))
    |> Array.map(fun (a,b) -> a.ToCharArray() |> Set, b.ToCharArray() |> Set)
    |> Array.map(fun (a,b) -> Set.intersect a b)
    |> Array.map Seq.exactlyOne
    |> Array.map priority
    |> Array.sum

let part2 = 
    input
    |> Array.chunkBySize 3
    |> Array.map(fun r -> r |> Array.map Set |> Set.intersectMany |> Seq.exactlyOne)
    |> Array.map priority
    |> Array.sum