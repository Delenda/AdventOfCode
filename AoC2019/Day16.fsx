let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ +   @"\Input\Day16.txt") 

let basePattern = [|0;1; 0; -1|]

let phase number iteration = 
    let reducedNumber = number%(4*iteration)
    let idx = reducedNumber/iteration
    basePattern.[idx]

let fftStep signal =
    Seq.init (signal |> List.length) (fun i -> i + 1)
    |> Seq.map(fun iteration -> 
        signal 
            |> Seq.skip (iteration-1)
            |> Seq.mapi(fun i d -> d * (phase (i + iteration ) iteration) ) 
            |> Seq.sum 
            |> fun d -> d%10
            |> abs)
    |> Seq.toList

let createSignal (str:string) =
    str.ToCharArray() |> Array.map string |> Array.map int |> Array.toList

let testSignal1 = createSignal "12345678"
let testSignal2 = createSignal "80871224585914546619083218645595"
let testSignal3 = createSignal "19617804207202209144916044189917"
let testSignal4 = createSignal "69317163492948606335995924319873"

let fft n signal = 
    signal
    |> (List.replicate n fftStep |> List.reduce (>>))

let test1 = fft   4 testSignal1 |> List.take 8 |> Seq.toList = [0; 1; 0; 2; 9; 4; 9; 8]
let test2 = fft 100 testSignal2 |> List.take 8 |> Seq.toList = [2; 4; 1; 7; 6; 1; 7; 6]
let test3 = fft 100 testSignal3 |> List.take 8 |> Seq.toList = [7; 3; 7; 4; 5; 4; 1; 8]
let test4 = fft 100 testSignal4 |> List.take 8 |> Seq.toList = [5; 2; 4; 3; 2; 1; 3; 3]

let formatSignal : int seq -> string= (Seq.take 8) >> (Seq.map string) >> (String.concat "")

let part1 = 
    let signal = createSignal input
    fft 100 signal
    |> formatSignal


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
    |> formatSignal
    
let test5 = messagePart2 "03036732577212944063491565474664" = "84462026"
let test6 = messagePart2 "02935109699940807407585447034323" = "78725270"
let test7 = messagePart2 "03081770884921959731165446850517" = "53553731"

let part2 = messagePart2 input