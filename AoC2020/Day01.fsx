let input =
    System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\Input\Day01.txt")
    |> Array.map int

let part1 =
    seq{
        for i in [0..input.Length-2] do
            for j in [i+1..input.Length-1] do
                if input.[i]+input.[j] = 2020 then yield input.[i]*input.[j]}
    |> Seq.head


let part2 =
    seq{
        for i in [0..input.Length-3] do
            for j in [i+1..input.Length-2] do
                for k in [j+1..input.Length-1] do
                if input.[i]+input.[j]+input.[k] = 2020 then yield input.[i]*input.[j]*input.[k]}
    |> Seq.head