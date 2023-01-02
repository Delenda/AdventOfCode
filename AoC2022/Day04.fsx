let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day04.txt") 

open System.Text

let assignments line = 
    let pattern = @"(\d+)-(\d+),(\d+)-(\d+)"
    let regex  = RegularExpressions.Regex(pattern)
    let m = regex.Match(line)
    let l1 = m.Groups.[1].Value |> int
    let r1 = m.Groups.[2].Value |> int
    let l2 = m.Groups.[3].Value |> int
    let r2 = m.Groups.[4].Value |> int
    (l1,r1),(l2,r2)

let contained line = 
    let (l1,r1),(l2,r2) = assignments line
    (l1 >= l2 && r1 <= r2) || (l1 <= l2 && r1 >= r2)
    
let overlap line = 
    let (l1,r1),(l2,r2) = assignments line
    (l2 <= r1 && r1 <= r2) ||(l2 <= l1 && l1 <= r2 ) || (contained line)

let part1 = 
    input 
    |> Array.filter contained
    |> Array.length

let part2 = 
    input
    |> Array.filter overlap
    |> Array.length