let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt") 

let search (msg:string) n =
    msg.ToCharArray()
    |> Seq.windowed n
    |> Seq.map Set
    |> Seq.takeWhile(fun s -> s.Count < n)
    |> Seq.length
    |> (+) n

let part1 = search input 4
let part2 = search input 14