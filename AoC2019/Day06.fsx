let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day06.txt") 

let parse (s:string)=
    let m = System.Text.RegularExpressions.Regex("(.{3})\)(.{3})").Match s
    m.Groups.[1].Value, m.Groups.[2].Value

let part1 = 
    let orbits = 
        input
        |> Array.map parse
        |> Array.groupBy fst
        |> Array.map(fun (a,b) -> a, b |> Array.map snd)
        |> Map

    let rec totalDepth level (a:string)  = 
        if orbits.ContainsKey a then 
            level + (orbits.[a] |> Array.sumBy(totalDepth (level + 1)))
        else
            level
    
    totalDepth 0 "COM"

let part2 = 
    let links = 
        input 
        |> Array.map parse
        |> Array.map(fun (a,b) -> b,a)
        |> Map

    let rec pathToCOM x = 
        if x = "COM" then 
            ["COM"] 
        else 
            x::(pathToCOM links.[x])

    let distance path = 
        path
        |> List.tail
        |> List.indexed 
        |> List.map(fun (a,b) -> (b,a)) 
        |> Map

    let pathYOU = pathToCOM "YOU"
    let pathSAN = pathToCOM "SAN"

    let distYOU = distance pathYOU
    let distSAN = distance pathSAN

    Set.intersect (Set pathYOU) (Set pathSAN) 
    |> Seq.map(fun x -> distYOU.[x] + distSAN.[x])
    |> Seq.min