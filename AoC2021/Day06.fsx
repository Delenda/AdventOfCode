let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt")

let lanternFish = input.Split(',') |> Array.map int |> Array.toList

//counts total number of offspring in a given period of days for a 
//single newly spawned lanternfish (the count includes itself) 
let spawnCount = 
    let init = [1..9] |> List.map(fun periodLength -> periodLength, 1UL) |> Map
    let maxPeriodLength = 300
    [10..maxPeriodLength]
    |> List.fold(fun (counter:Map<int,uint64>) periodLength -> 
        let count = 
            let subsequentSpawns = [periodLength-9..-7..1]
            1UL + (subsequentSpawns |> List.sumBy(fun d -> counter.[d]))
        counter.Add(periodLength,count)) init

let totalOffspring fish days = 
    fish |> List.sumBy(fun timer -> spawnCount.[days + (9-timer)])

let part1 = totalOffspring lanternFish 80
let part2 = totalOffspring lanternFish 256