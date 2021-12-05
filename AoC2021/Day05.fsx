let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt")

type line = {x1:int;y1:int;x2:int;y2:int}

let parseLine str= 
    let regex = System.Text.RegularExpressions.Regex(@"(\d+),(\d+) -> (\d+),(\d+)")
    let groups = regex.Match(str).Groups
    {x1 = groups.[1].Value |> int
     y1 = groups.[2].Value |> int
     x2 = groups.[3].Value |> int
     y2 = groups.[4].Value |> int}

let inputLines = input |> Array.map parseLine

let intersectionCount lines = 
    lines
    |> Seq.collect(fun  line->
        let length = if line.x1 = line.x2 then abs(line.y2-line.y1) else abs(line.x2-line.x1)
        let slopeX = if line.x1 = line.x2 then 0 else if line.x1 < line.x2 then 1 else -1
        let slopeY = if line.y1 = line.y2 then 0 else if line.y1 < line.y2 then 1 else -1
        seq{for delta in [0..length] do
            let x = line.x1+delta*slopeX 
            let y = line.y1+delta*slopeY
            yield x,y})
    |> Seq.countBy id
    |> Seq.filter(fun (point,count) -> count > 1)
    |> Seq.length

let part1 = inputLines |> Seq.filter(fun line -> line.x1 = line.x2 || line.y1 = line.y2) |> intersectionCount
let part2 = inputLines |> intersectionCount