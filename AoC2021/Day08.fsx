let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day08.txt")

type DisplayInfo = 
    {Segments : Map<int, Set<Set<char>>>
     Display : string array}

let parseDisplayInfo (str:string) =
    match str.Split('|') with
    | [|digitpart; displaypart|] ->
        let segments = 
            digitpart.Split([|" "|],System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.groupBy(fun (s:string) -> s.Length)
            |> Map
            |> Map.map(fun _ k -> k|> Array.map(fun s -> s.ToCharArray() |> Set) |> Set)

        let display = 
            displaypart.Split([|" "|], System.StringSplitOptions.RemoveEmptyEntries)

        {Segments = segments; Display = display}

    | _ -> failwith "unexpected string"

let displayInfo = input |> Array.map parseDisplayInfo

let part1 = 
    displayInfo
    |> Array.sumBy(fun di -> di.Display |> Seq.filter(fun d -> d.Length = 2 || d.Length = 3 || d.Length = 4 || d.Length = 7) |> Seq.length)

let readDisplay (displayInfo: DisplayInfo) =
    let pick count criteria = displayInfo.Segments.[count] |> Seq.filter(criteria) |> Seq.exactlyOne
    let segments1 = pick 2 (fun _ -> true)
    let segments4 = pick 4 (fun _ -> true)
    let segments7 = pick 3 (fun _ -> true)
    let segments8 = pick 7 (fun _ -> true)
    let segments6 = pick 6 ((Set.isSubset segments1) >> not)
    let segments9 = pick 6 (Set.isSubset segments4) 
    let segments3 = pick 5 (Set.isSubset segments1)
    let segments5 = pick 5 (Set.isSuperset segments6)
    let segments2 = pick 5 ((Set.isSuperset segments9) >> not) 
    let segments0 = pick 6 ((Set.isSubset segments5) >> not)
    
    let dictionary = 
        [segments0, 0
         segments1, 1
         segments2, 2
         segments3, 3
         segments4, 4
         segments5, 5
         segments6, 6
         segments7, 7
         segments8, 8
         segments9, 9] |> Map

    displayInfo.Display
    |> Array.map(fun segments -> segments.ToCharArray() |> Set)
    |> Array.map(fun segments -> dictionary.[segments])
    |> Array.reduce(fun a b -> 10*a + b)

let part2 = displayInfo |> Array.sumBy readDisplay