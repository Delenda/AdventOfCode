let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day10.txt") 

type destination = 
    | Robot of int
    | Output of int

type robotRule = 
    {
        Id : int
        High : destination
        Low : destination
    }

type chipPosition = 
    {
        Chip: int
        Position : destination
    }

type rule = 
    | RobotRule of robotRule
    | InputRule of chipPosition

let parse (s:string) =
    let robotMatch = System.Text.RegularExpressions.Regex("bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)").Match s
    let inputMatch = System.Text.RegularExpressions.Regex("value (\d+) goes to bot (\d+)").Match s
    if robotMatch.Success then
        let lowDest  = if robotMatch.Groups.[2].Value = "bot" then Robot else Output
        let highDest = if robotMatch.Groups.[4].Value = "bot" then Robot else Output
        {
            Id   = robotMatch.Groups.[1].Value |> int
            Low  = robotMatch.Groups.[3].Value |> int |> lowDest
            High = robotMatch.Groups.[5].Value |> int |> highDest
        }  |> RobotRule
    else if inputMatch.Success then
        {
            Chip     = inputMatch.Groups.[1].Value |> int
            Position = inputMatch.Groups.[2].Value |> int |> Robot
        } |> InputRule
    else
        failwith (sprintf "No parse: %s" s)

let isRobotRule = function
    | RobotRule _ -> true
    | InputRule _ -> false

let rules = 
    input 
    |> Array.map parse

let robotRules = 
    rules 
    |> Array.filter isRobotRule
    |> Array.map(fun rule -> 
        match rule with
        | RobotRule r -> r.Id, r
        | InputRule _ -> failwith "Unexpected rule")
    |> Map

let initialDestinations = 
    rules
    |> Seq.filter (isRobotRule >> not)
    |> Seq.map(fun rule -> 
        match rule with
        | InputRule d -> d
        | RobotRule _ -> failwith ("Unexpected rule"))
    |> Set

let stepProductionLine (stopcondition: Set<chipPosition> -> bool) (chipPositions:Set<chipPosition>) =
    if stopcondition chipPositions then None else

    let pairedChips=
        chipPositions
        |> Seq.groupBy(fun chip -> chip.Position)
        |> Seq.filter(fun (position,chips) -> chips |> Seq.length = 2)

    let unpairedChips = 
        chipPositions
        |> Seq.groupBy(fun chip -> chip.Position)
        |> Seq.filter(fun (position,chips) -> chips |> Seq.length = 1)
        |> Seq.collect snd
        |> Set

    let newDestinations (destination, chipPair) =
        let robotId = 
            match destination with
            | Robot id -> id
            | Output _ -> failwith("unexpected destination")

        let lowChip  = chipPair |> Seq.map(fun cp -> cp.Chip) |> Seq.min
        let highChip = chipPair |> Seq.map(fun cp -> cp.Chip) |> Seq.max
        [
            {Chip = lowChip;  Position =  robotRules.[robotId].Low}
            {Chip = highChip; Position =  robotRules.[robotId].High}
        ] |> Set
            
    let newChipPositions = 
        pairedChips
        |> Seq.map newDestinations
        |> Set.unionMany
        |> Set.union unpairedChips

    Some(newChipPositions, newChipPositions)
    
let commonDestination_for_Chip61_and_Chip17 (chipPositions:Set<chipPosition>) = 
    chipPositions
    |> Seq.groupBy(fun chip -> chip.Position)
    |> Seq.tryPick(fun (dest, chips) -> 
        if chips |> Seq.map(fun cp -> cp.Chip) |> Set = (Set [61;17]) then Some dest else None )

let Chip61_and_Chip17_at_same_destination chipPositions = 
    (commonDestination_for_Chip61_and_Chip17 chipPositions).IsSome
   
let chips_exist_at_output0_and_output1_and_output2 chipPositions = 
    let destinations = chipPositions |> Set.map(fun cp -> cp.Position)
    destinations.Contains (Output 0) && destinations.Contains (Output 1) && destinations.Contains (Output 2)

let part1 = 
    Seq.unfold (stepProductionLine Chip61_and_Chip17_at_same_destination) initialDestinations
    |> Seq.last
    |> commonDestination_for_Chip61_and_Chip17

let part2 = 
    Seq.unfold (stepProductionLine chips_exist_at_output0_and_output1_and_output2) initialDestinations
    |> Seq.last
    |> Seq.map(fun cp -> 
        match cp.Position with
        | Output 0 | Output 1 | Output 2 -> cp.Chip
        | _ -> 1)
    |> Seq.reduce (*)