let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day07.txt")

let totalFuel fuelMetric = 
    let crabs = input.Split(',') |> Array.toList |> List.map int
    let max = crabs |> List.max
    let min = crabs |> List.min
    [min..max] |> List.map(fun destination -> crabs |> List.sumBy(fun crabPostion -> fuelMetric destination crabPostion)) |> List.min

let dist d c = abs(d-c)
let triangleDist d c = abs(d-c) * (abs(d-c)+1) / 2

let part1 = totalFuel dist
let part2 = totalFuel triangleDist