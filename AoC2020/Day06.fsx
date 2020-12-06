let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt")

let answers = 
    let split p s = System.Text.RegularExpressions.Regex.Split(s,p)
    split @"\r\n\r\n" input
    |> Seq.map(split @"\r\n")
    |> Seq.map (Seq.map Set)

let count setfunction = 
    answers
    |> Seq.map setfunction 
    |> Seq.sumBy Seq.length

let part1 = count Set.unionMany
let part2 = count Set.intersectMany