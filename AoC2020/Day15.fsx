let input =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day15.txt")
        .Split(',')
        |> Array.toList
        |> List.map int

let play n startingnumbers =
    let cache = new System.Collections.Generic.Dictionary<int,int>()
    startingnumbers |> List.indexed |> List.iter(fun (i,c) -> cache.Add(c,i))
    Seq.initInfinite(fun i -> i + startingnumbers.Length - 1)
    |> Seq.scan (fun last lastIdx ->
        let newNumber =
            if cache.ContainsKey last then
                lastIdx - cache.[last]
            else
                0
        if cache.ContainsKey last then (cache.Remove last |> ignore)
        cache.Add(last, lastIdx)
        newNumber) -1
    |> Seq.tail
    |> Seq.take (n - startingnumbers.Length )
    |> Seq.last

let part1 = play 2020 input
let part2 = play 30000000 input