let input =
    System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day15.txt")
        .Split(',')
        |> Array.toList
        |> List.map int

let play  n startingnumbers =
    startingnumbers
    |> List.rev
    |> Seq.unfold(fun spoken ->
        let lastSpoken = List.head spoken
        let newNumber =
            spoken
            |> List.tail
            |> List.tryFindIndex(fun t -> t = lastSpoken)
            |> Option.map(fun t -> t + 1)
            |> Option.defaultValue 0
        Some(newNumber,newNumber::spoken))
    |> Seq.take (n - startingnumbers.Length)
    |> Seq.last



let play2  n startingnumbers =
    let cache = new System.Collections.Generic.Dictionary<int,int>()
    startingnumbers |> List.indexed |> List.iter(fun (i,c) -> cache.Add(i,c))
    let mutable last = -1
    Seq.initInfinite(fun i ->
        let idx = i + startingnumbers.Length
        let newNumber =
            if cache.ContainsKey last then
                idx - cache.[last]
            else
                0
        if cache.ContainsKey last then (cache.Remove last |> ignore)
        cache.Add(last, idx - 1)
        last <- newNumber
        newNumber)
    |> Seq.take (n - startingnumbers.Length)
    |> Seq.last

let part1 = play2 2020 [0;3;6]
//let part2 = play 300000 input
