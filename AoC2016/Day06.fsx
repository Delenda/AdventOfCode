let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt") 

let message (sorter: (char*int) array -> (char*int) array) = 
    let arr = input  |> Array.map(fun s -> s.ToCharArray())
    [0..7] 
    |> List.map(fun i -> arr |> Array.map(fun x -> x.[i]))
    |> List.map(Array.countBy id)
    |> List.map(sorter)
    |> List.map(Array.head)
    |> List.map(fst)
    |> List.map(string)
    |> String.concat ""

let part1 = message (Array.sortByDescending snd)
let part2 = message (Array.sortBy snd)