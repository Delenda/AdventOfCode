let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day09.txt") 

type compression = 
    {
        Length : uint64
        Repitition : uint64
        String : string
    }
 
let regex = System.Text.RegularExpressions.Regex "\((\d+)x(\d+)\)"

let rec parse (s:string)= 
    let m = regex.Match s
    if m.Success then
        let idx = m.Index
        let compressDescription = m.Groups.[0].Value
        let length = m.Groups.[1].Value |> int
        let repitition = m.Groups.[2].Value |> int

        let initialPart = 
            if idx > 0 then 
                [{Length = uint64 idx; Repitition = uint64 1; String = s.Substring(0,idx)}] 
            else 
                []

        let compressedPart = 
            [{Length = uint64 length; Repitition = uint64 repitition; String = s.Substring(idx + compressDescription.Length, length) }]

        let remainingPart = 
            if idx + compressDescription.Length + length < s.Length then 
                parse (s.Substring(idx + compressDescription.Length + length)) 
            else 
                []

        initialPart @ compressedPart @ remainingPart
    else
        [{Length = uint64 s.Length; Repitition = uint64 1; String = s}]

let part1 = 
    parse input
    |> List.sumBy(fun x -> x.Length*x.Repitition)
 
let rec size comp = 
    let m = regex.Match (comp.String)
    if m.Success then
        parse comp.String
        |> List.sumBy size
        |> (*) comp.Repitition
    else
        comp.Length * comp.Repitition

let part2 = 
    input
    |> parse
    |> List.sumBy size