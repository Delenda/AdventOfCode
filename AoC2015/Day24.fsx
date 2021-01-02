let weights = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day24.txt")
    |> Seq.map int
    |> Seq.toList

let rec partition target wgts : int list list=
    match wgts with
    | [] -> []
    | x::xs -> 
        if x = target then [[x]] else
        List.append (partition target xs) ((partition (target - x) xs) |> List.map(fun l -> x::l))

let quantumEntanglement numberOfCompartments = 
    let target_weight = weights |> Seq.sum |> fun s -> s/numberOfCompartments
    let partitions = partition target_weight weights |> List.map Set
    let frontCount = partitions |> Seq.map (fun s -> s.Count) |> Seq.min
    partitions
    |> Seq.filter(fun s -> s.Count = frontCount)
    |> Seq.map(fun s -> s |> Seq.map uint64 |> Seq.reduce (*))
    |> Seq.min

let part1 = quantumEntanglement 3
let part2 = quantumEntanglement 4