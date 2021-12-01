let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day01.txt") |> Seq.map int

let countIncrease = 
    Seq.windowed 2
    >> Seq.filter(fun pair -> pair.[1] > pair.[0])
    >> Seq.length

let part1 = 
    input
    |> countIncrease

let part2 = 
    input
    |> Seq.windowed 3
    |> Seq.map Array.sum
    |> countIncrease