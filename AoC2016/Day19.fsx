let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day19.txt") |> int

let part1 = 
    let initial = 
        Seq.init input (fun i -> i, i+1)
        |> Seq.tail
        |> Map
        |> fun m -> m.Add(input, 1)
    Seq.unfold(fun ((current, state): int*Map<int,int>) -> 
        let newCurrent = state.[state.[current]]
        if current = newCurrent then None else
        let newState = state.Add(current, newCurrent)
        Some(newCurrent, (newCurrent, newState))
        ) (1, initial)
    |> Seq.last
        
let part2 = 
    let elfCount = input
    let initial = 
        Seq.init elfCount (fun i -> i, i+1)
        |> Seq.tail
        |> Map
        |> fun m -> m.Add(elfCount, 1)
    Seq.unfold(fun ((current, opposite, parity,  state): int*int*int*Map<int,int>) -> 
        let newState = state.Add(opposite, state.[state.[opposite]])
        let newCurrent = newState.[current]
        if current = newCurrent then None else
        let newOpposite = if parity = 0 then opposite else newState.[opposite]
        Some(newCurrent, (newCurrent, newOpposite, 1 - parity, newState))
        ) (1, elfCount/2 ,elfCount%2, initial)
    |> Seq.last