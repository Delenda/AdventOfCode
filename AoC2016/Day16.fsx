let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day16.txt")

let iteration (a:int list) = 
    a@[0]@(a |> List.rev |> List.map(fun x -> 1 - x))

let initial = 
    input.ToCharArray() 
    |> Seq.map(fun c -> int c - int '0')
    |> Seq.toList

let rec checksum (data : int list)= 
    if data.Length%2 = 1 then data else
    data 
    |> List.chunkBySize 2
    |> List.map(fun l -> if l.Head = l.Tail.Head then 1 else 0)
    |> checksum
    
let fillDisk size = 
    Seq.initInfinite id
    |> Seq.scan (fun s _ -> iteration s) initial
    |> Seq.skipWhile(fun l -> l.Length < size)
    |> Seq.head
    |> List.take size
    |> checksum
    |> List.map string
    |> String.concat ""

let part1 = fillDisk 272
let part2 = fillDisk 35651584 // ~30 seconds