let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day01.txt") |> Array.head

let question1 =
    let counts = input.ToCharArray() |> Array.countBy id |> Map.ofArray
    counts.['('] - counts.[')']

let question2 = 
    let step (pos,idx) char = 
        match char with
        | '(' -> (pos + 1, idx + 1)
        | ')' -> (pos - 1, idx + 1)
        | _ -> failwithf "unexpected character: %c" char
    input.ToCharArray() |> Array.scan step (0,0) |> Seq.filter(fun x -> fst x < 0) |> Seq.head |> snd