let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ +   @"\Input\Day16.txt") 

let messagePart2 (str:string) =
    let signal = str.ToCharArray() |> Array.map string |> Array.map int
    let offset = str.Substring(0,7) |> int
    let subSignalLength = str.Length * 10000 - offset
    let reverseInput = signal |> Array.rev

    let reverseSignal = 
        Seq.init subSignalLength id
        |> Seq.map(fun i -> reverseInput.[i%reverseInput.Length])
        |> Seq.toArray

    reverseSignal
    |> (List.replicate 100 ( (Array.scan(fun s t -> (s + t)%10 ) 0) >> Array.tail)  |> List.reduce(>>))
    |> Seq.rev
    |> Seq.take 8
    |> Seq.map string
    |> String.concat ""
    
let test1 = messagePart2 "03036732577212944063491565474664" = "84462026"
let test2 = messagePart2 "02935109699940807407585447034323" = "78725270"
let test3 = messagePart2 "03081770884921959731165446850517" = "53553731"

let part2 = messagePart2 input