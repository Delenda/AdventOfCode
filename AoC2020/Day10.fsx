let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day10.txt") |> Array.map int

let part1 =
    input
    |> Seq.sort
    |> Seq.windowed 2
    |> Seq.countBy(fun a -> Seq.last a - Seq.head a ) |> Seq.toList
    |> Map
    |> fun m -> (m.[1] + 1)*(m.[3] + 1)

let part2 =
    let slices =
        input
        |> Seq.sort
        |> Seq.fold(fun s t ->
            if List.head (fst s) +  3 = t then
                [t], fst s :: snd s
            else
                t::(fst s), snd s
            ) ([0], [])
        |> fun (a,b) -> a::b

    let rec length a =
        match a with
        | x::y::z::w::_ ->
            if w-x < 4 then
                length (List.skip 1 a) + length (List.skip 2 a) + length (List.skip 3 a)
            elif z-x < 4 then
                length (List.skip 1 a) + length (List.skip 2 a)
            else
                length (List.skip 1 a)
        | x::y::z::[] ->
            if z-x < 4 then
                2
            else
                1
        | _ -> 1

    slices
    |> List.map List.sort
    |> List.map length
    |> List.map uint64
    |> List.reduce (*)