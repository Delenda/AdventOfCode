let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day11.txt")

type operation = | Plus of uint64 | Multiply of uint64 | Square

type monkey = 
    {
        Id : int
        Items : uint64 list
        Inspection : operation
        Test: uint64
        TestTrue : int
        TestFalse : int
    }

let parseMonkey (m: string array) =
    let id = m.[0].Substring(7,1) |> int
    let items = m.[1].Split(':').[1].Split(',') |> Array.map(fun s -> s.Replace(" ","")) |> Array.map uint64 |> Array.toList
    let operation =
        if m.[2].Split(' ') |> Array.last = "old" then Square else
        let lv = m.[2].Split(' ') |> Array.last |> uint64
        let op = m.[2].Substring(23,1) 
        match op with
        | "+" -> Plus lv
        | "*" -> Multiply lv
        | _ -> op |> sprintf "unexpected character: %s" |> failwith
    let test = m.[3].Split(' ') |> Array.last |> uint64
    let testtrue  = m.[4].Split(' ') |> Array.last |> int
    let testfalse = m.[5].Split(' ') |> Array.last |> int
    {
        Id = id
        Items = items
        Inspection = operation
        Test = test
        TestTrue = testtrue
        TestFalse = testfalse
    }

let monkeys = 
    input
    |> Array.chunkBySize 7
    |> Array.map parseMonkey
    |> Array.map (fun m -> m.Id, m)
    |> Map

type state = { Possessions : Map<int, int list>; WorryLevels: Map<int,uint64>; Round : int; CurrentMonkey : int; InspectionCount : Map<int,uint64>}

let modulo = 
    monkeys
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map (fun m -> m.Test)
    |> Seq.reduce (*)

let newWorryLevel wlReduction op old = 
    let inspection = 
        match op with 
        | Plus a -> old + a
        | Multiply m -> old * m
        | Square -> old * old
    inspection/wlReduction

let round wlReduction state = 
    let monkey = monkeys.[state.CurrentMonkey]
    let items = state.Possessions.[state.CurrentMonkey]
    let newPosssesions, newWorryLevels = 
        List.rev items
        |> List.fold(fun ((pos,wl):Map<int, int list> * Map<int, uint64>) t -> 
            let newWorryLevel = (newWorryLevel wlReduction monkey.Inspection wl.[t])%modulo
            let recipient = 
                if newWorryLevel % monkey.Test = 0UL then
                    monkey.TestTrue
                else 
                    monkey.TestFalse
            let newWl = wl.Add(t, newWorryLevel)
            let newPos = pos.Add(recipient, t::pos.[recipient])
            newPos, newWl
            ) (state.Possessions, state.WorryLevels)
    let newMonkey = (state.CurrentMonkey + 1)%(monkeys.Count)
    let newRound  = if newMonkey = 0 then state.Round + 1 else state.Round
    let newInspectionCount = 
        let newCount = state.InspectionCount.[monkey.Id] + uint64(items.Length)
        state.InspectionCount.Add(monkey.Id, newCount)
    let newState = {Possessions = newPosssesions.Add(monkey.Id, []); WorryLevels = newWorryLevels; CurrentMonkey = newMonkey; Round = newRound; InspectionCount = newInspectionCount }
    Some(newState,newState)

let initialState = 
    let possession, worrylevels = 
        monkeys
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sortBy(fun m -> m.Id)
        |> Seq.fold(fun ((p,w):Map<int,int list> * Map<int,uint64>) monkey -> 
            let newStartIdx = w.Count
            let items = 
                monkey.Items
                |> List.indexed
                |> List.map(fun (idx,x) -> (newStartIdx + idx, x))
            let newW = 
                items
                |> List.fold(fun (s:Map<int,uint64>) t -> s.Add t) w
            let newP = 
                p.Add(monkey.Id, items |> List.rev |> List.map fst)
            newP, newW
            ) (Map.empty, Map.empty)
    let inspectionCount = 
        monkeys
        |> Map.map(fun _ _ -> 0UL)
    {Possessions = possession; WorryLevels = worrylevels; CurrentMonkey = 0; Round = 1; InspectionCount = inspectionCount}

let monkeyBusiness wlReduction rounds= 
    Seq.unfold (round wlReduction) initialState
    |> Seq.skipWhile(fun s -> s.Round < rounds + 1)
    |> Seq.head
    |> fun s -> s.InspectionCount
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sortDescending
    |> Seq.toArray
    |> fun m -> m.[0] * m.[1]
    
let part1 = monkeyBusiness 3UL 20
let part2 = monkeyBusiness 1UL 10000