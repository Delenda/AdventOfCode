let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day19.txt")

type ruleType = 
    | Literal of string
    | Composite of int list list

let isLiteral = function
    | Literal _ -> true
    | _ -> false

let parseRules (line:string) = 
    let parts = line.Split(':')
    let ruleNumber = int parts.[0]
    if parts.[1].Contains "\"" then
        let literal = parts.[1].Trim().Replace("\"","")
        ruleNumber, Literal literal
    else
        let composites = 
            parts.[1].Split('|')
            |> Array.map(fun s -> s.Trim().Split(' '))
            |> Array.map (Array.map int)
            |> Array.map(Array.toList)
            |> Array.toList
            |> Composite
        ruleNumber, composites

let messages = 
    input
    |> Array.filter(fun s -> s.Contains ":" |> not)
    |> Array.filter(fun s -> s.Length > 0)

let rules = 
    input
    |> Array.filter(fun s -> s.Contains ":")
    |> Array.map parseRules
    |> Map

let needRules = function
    | Literal _ -> Set.empty
    | Composite lists -> lists |> List.collect id |> Set

let extractString = function
    | Literal a -> a
    | _ -> failwith "Not a string"

let rec combineLists (lists: string list list) : string list= 
    match lists with
    | [] -> [""]
    | x::xs -> combineLists xs |> List.collect(fun y -> x |> List.map(fun z -> z+y)) 

let resolveRule (literals:Map<int, string list>) (composite: int list list)  : string list = 
    composite
    |> List.map(fun rules -> rules |> List.map(fun rule -> literals.[rule]))
    |> List.collect combineLists
    |> List.distinct

let compositions = function
    | Literal _ -> []
    | Composite k -> k

let literalRules = 
    let initialLiterals = 
        rules 
        |> Map.filter(fun k v -> isLiteral v)
        |> Map.map(fun k v -> [extractString v])
    let neededRules = 
        rules
        |> Map.filter(fun k v -> isLiteral v |> not)
        |> Map.map(fun k v -> needRules v)
    let initialComposites = 
        neededRules 
        |> Map.filter(fun k v -> v.IsEmpty |> not) 
        |> Seq.map(fun kvp -> kvp.Key) 
        |> Set
    Seq.unfold(fun ((literals, composites): Map<int, string list>*Set<int>) ->
        if composites.IsEmpty then None else
        let literalRules = literals |> Seq.map(fun kvp -> kvp.Key) |> Set
        let resolvable = 
            composites
            |> Set.filter(fun rule -> Set.isSubset neededRules.[rule] literalRules)
        let resolved=
            resolvable
            |> Seq.map(fun rule -> rule, rules.[rule] |> compositions |> (resolveRule literals))
        let newComposites = Set.difference composites resolvable
        let newLiterals =
            resolved
            |> Seq.fold(fun (s:Map<int,string list>) (t: int* string list) -> s.Add t) literals
        Some(newLiterals, (newLiterals, newComposites))
        ) (initialLiterals, initialComposites)
    |> Seq.last

let part1 = 
    let rule0  = literalRules.[0] |> Set
    messages |> Seq.filter(fun m -> rule0.Contains m) |> Seq.length

let part2 = 
    let rule42 = literalRules.[42] |> Set
    let rule31 = literalRules.[31] |> Set
    messages
    |> Array.map(fun msg ->     
        msg
        |> Seq.chunkBySize 8
        |> Seq.map(Seq.map string) 
        |> Seq.map(String.concat "")
        |> Seq.toArray)
    |> Array.map(fun a -> 
        a |> Array.map(fun s -> 
            if rule42.Contains s && rule31.Contains s then
                failwith "overlap"
            else if rule42.Contains s then
                "A"
            else if rule31.Contains s then 
                "B"
            else failwith "string doesnt match 8 og 31")
        |> String.concat "")
    |> Seq.filter(fun s -> s.Contains "BA" |> not)
    |> Seq.filter(fun s -> 
        let aCount = s.ToCharArray() |> Seq.filter(fun c -> c = 'A') |> Seq.length
        let bCount = s.ToCharArray() |> Seq.filter(fun c -> c = 'B') |> Seq.length
        aCount > bCount && bCount > 0)
    |> Seq.length