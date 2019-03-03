let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt")
let blocks = input |> Array.head |> fun s -> s.Split('\t') |> Array.map int

let redistribute mbs =
    let block = mbs |> Array.last
    let max = block |> Array.max
    let idx = block |> Array.findIndex(fun b -> b = max)
    let mutable newBlock = block |> Array.map id
    newBlock.[idx] <- 0
    [1..max] |> List.iter(fun i -> newBlock.[(idx + i) % 16]<-newBlock.[(idx + i) % 16] + 1)
    match mbs |> Array.contains newBlock with
    | true -> None
    | false -> Some (newBlock , Array.concat [|mbs; [|newBlock|]|] )

let a  = Seq.unfold(fun s -> redistribute s) [|blocks|] |> Array.ofSeq
a.Length + 1 // Question 1

let b = match redistribute [| a |> Array.last|] with
        | Some (x,y) -> x
        | None -> [||]
let idx = a |> Array.findIndex(fun s -> s = b)
a.Length - idx // Question 2