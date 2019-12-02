let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day01.txt")

let part1 = 
    input
    |> Array.map int
    |> Array.sumBy(fun mass -> mass/3 - 2)

let rec totalfuel mass =
    let fuel = mass/3 - 2
    if fuel < 0 then 0 else fuel + totalfuel fuel

let part2 = 
    input
    |> Array.map int
    |> Array.sumBy totalfuel