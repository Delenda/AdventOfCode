let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day01.txt")|> Array.head

let findSum offset = 
    let digits = input.ToCharArray() |> Array.map (string >> int)

    let pairedDigitIndex digit = (digit + offset) % digits.Length

    [0..digits.Length-1] 
        |> List.map(fun i -> (i, pairedDigitIndex i))
        |> List.map(fun (i,j) -> (digits.[i], digits.[j])) 
        |> List.filter(fun x -> fst x = snd x) 
        |> List.sumBy fst

let question1 = findSum 1
let question2 = findSum (input.Length/2)