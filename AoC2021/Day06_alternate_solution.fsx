let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt")

let lanternFish = input.Split(',') |> Array.map int |> Array.toList

let rec spawn remainingDays (population:Map<int,uint64>) =
    if remainingDays = 0 then population |> Map.toSeq |> Seq.sumBy snd else
    let lookup n = population.TryFind n |> Option.defaultValue 0UL
    let nextGeneration = 
        [8, lookup 0
         7, lookup 8
         6, lookup 7 + lookup 0
         5, lookup 6
         4, lookup 5
         3, lookup 4
         2, lookup 3
         1, lookup 2
         0, lookup 1] |> Map
    spawn (remainingDays - 1) nextGeneration

let initialCount = lanternFish |> List.countBy id |> Map |> Map.map(fun _ v -> uint64 v) 
let part1 = spawn 80  initialCount
let part2 = spawn 256 initialCount