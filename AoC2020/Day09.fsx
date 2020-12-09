let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day09.txt") |> Array.map uint64


let part1 =
    let idxs =
        seq {for i in [1..24] do
                for j in [(i+1)..25] do
                    if i<>j then yield (i,j) }
        |> Seq.toList

    seq{for k in [25..(input.Length-1)] do
        let sumExists =
            idxs
            |> Seq.map(fun (i,j) -> input.[k-i] + input.[k-j])
            |> Seq.exists(fun x -> x = input.[k])
        if not sumExists then yield input.[k]
        }
    |> Seq.head

let part2 =
    let totalSum = input |> Array.sum
    let leftSums, rightSums =
        let left, right =
            input
            |> Array.scan(fun (left,right) t -> (left + t, right - t)) (0UL, totalSum)
            |> Array.unzip
        left |> Array.tail, right |> Array.take 1000
    let terminate,start =
        seq{for j in [1..(leftSums.Length-2)] do
                for i in [j+1..(leftSums.Length-1)] do
                    if leftSums.[i] + rightSums.[j] - totalSum = part1 then yield (i,j)}
        |> Seq.head
    let segment = [start..terminate] |> List.map(fun i -> input.[i])
    (List.max segment) + (List.min segment)