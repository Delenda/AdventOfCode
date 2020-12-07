let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day07.txt")

open System.Text.RegularExpressions
let pattern = @"([\w|\s]+) bags contain(\s?\d+[\w|\s]+bags?,?\s?)+."

let bagrules (f:string)= 
    seq {for m in Regex.Matches(f.Replace("\r\n", " "), pattern) do
           for c in m.Groups.[2].Captures do
           let cs = 
                c.Value.Replace(", ","").Trim().Replace(" bags","").Replace(" bag", "").Split(' ')
           let count = cs.[0].Trim() |> int
           yield  m.Groups.[1].Value.Trim().Replace("bags","bag"), (count, cs |> Array.tail |> String.concat " " )}
    |> Seq.toList
  
let part1 = 
    let rules = 
        bagrules input
        |> Seq.map(fun (a, (_,b)) -> b, a)
        |> Seq.groupBy fst
        |> Seq.map(fun (a,b) -> a, b |> Seq.map snd)
        |> Map
              
    let initialFrontier = Set ["shiny gold"]
    let initialSeen = Set.empty
    let connectedComponent = 
        (initialFrontier, initialSeen)
            |> Seq.unfold(fun ((frontier, seen) : (Set<string>*Set<string>)) ->
                if frontier.IsEmpty then None else
                let t = frontier |> Seq.head
                if rules.ContainsKey t |> not then
                    let newSeen = seen.Add t
                    let newFrontier = frontier.Remove t
                    Some(newSeen, (newFrontier, newSeen))
                else
                    let candidates = rules.[t] |> Seq.filter(fun g -> seen.Contains g |> not) |> Set
                    let newFrontier = frontier.Remove t |> Set.union candidates
                    let newSeen =  Set.union seen candidates |> Set.union (Set [t])
                    Some(newSeen, (newFrontier, newSeen))) 
            |> Seq.last
    (connectedComponent.Count - 1)

let part2 = 
    let rules =
        bagrules input
        |> Seq.groupBy fst
        |> Seq.map(fun (a,b) -> a, b |> Seq.map snd)
        |> Map
    let rec count t = 
        if rules.ContainsKey t |> not then 0 else rules.[t]  |> Seq.sumBy(fun x ->(fst x) + (fst x) * (count (snd x)))
    count "shiny gold"