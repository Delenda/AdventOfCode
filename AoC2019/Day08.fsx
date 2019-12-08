let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day08.txt") 

let layers = input.ToCharArray() |> Array.chunkBySize 150

let part1  = 
    layers
    |> Array.sortBy (Array.filter(fun c -> c = '0') >> Array.length)
    |> Array.head
    |> Array.groupBy id
    |> Map
    |> fun m -> (m.['1'] |> Array.length) * (m.['2'] |> Array.length)

let part2 = 
    seq{for pixel in [0..149] do
        yield
            layers
            |> Seq.map(fun layer -> layer.[pixel])
            |> Seq.skipWhile(fun c -> c  = '2')
            |> Seq.head
            |> fun c -> if c = '1' then "#" else " "
    }
    |> Seq.chunkBySize 25
    |> Seq.map (String.concat "")
    |> Seq.iter System.Console.WriteLine