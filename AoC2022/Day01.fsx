let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day01.txt") 

let totalCalories = 
    input
    |> Array.fold (fun (cals,count) t -> 
        if t = "" then 
            (count::cals),0 
        else
            cals, (count + (int t))
        ) ([],0)
    |> fst

let part1 = totalCalories |> List.max

let part2 = totalCalories |> List.sortDescending |> List.take 3 |> List.sum
    