let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day11.txt")

type componenttype = |Generator|Microchip    
type floor = |First|Second|Third|Fourth
type element = |Polonium|Thulium|Promethium|Ruthenium|Cobalt|Elerium|Dilithium
type Symbol = (floor*floor) list
type state = 
    {
        Elevator : floor
        Chips : Map<element, floor>
        Generators : Map<element,floor>
    }
    member this.Symbol : Symbol= 
        (this.Elevator, this.Elevator)::
            (this.Chips
            |> Seq.map(fun kvp -> kvp.Value, this.Generators.[kvp.Key] )
            |> Seq.sort
            |> Seq.toList)

let floor (line:string) =
    let m = System.Text.RegularExpressions.Regex.Match(line, @"^The (\w+) ")
    match m.Groups.[1].Value with
    | "first" -> First
    | "second" -> Second
    | "third" -> Third
    |  "fourth" -> Fourth
    | _ -> failwith ("Unexpected floor")

let parse_element = function
    | "polonium" -> Polonium
    | "thulium" -> Thulium
    | "promethium" -> Promethium
    | "ruthenium" -> Ruthenium
    | "cobalt" -> Cobalt
    | _ -> failwith ("Unexpected element")

let parse (line:string) =
    let floor = floor line
    let pattern = @"(polonium|thulium|promethium|ruthenium|cobalt)(-)?"
    seq{for m in System.Text.RegularExpressions.Regex.Matches(line, pattern) do yield (floor, m.Groups.[1].Value |>parse_element,if m.Groups.[2].Value = "" then Generator else Microchip)}
    |> Seq.toArray

let initialState = 
    let positions = input |> Array.collect parse
    let generators = positions |> Array.filter(fun (_,_,x) -> x = Generator) |> Array.map(fun (a,b,_) -> b,a)|> Map
    let microchips = positions |> Array.filter(fun (_,_,x) -> x = Microchip) |> Array.map(fun (a,b,_) -> b,a) |> Map
    {
        Elevator = First
        Generators = generators
        Chips = microchips
    }

let adjacentFloor = function
    | First -> [Second]
    | Second -> [Third;First]
    | Third -> [Fourth;Second]
    | Fourth -> [Third]

let move newFloor state (chips,generators) = 
    let newChips = chips |> List.fold(fun (s:Map<element,floor>) t -> s.Add(t, newFloor)) state.Chips
    let newGens = generators |> List.fold(fun (s:Map<element,floor>) t -> s.Add(t, newFloor)) state.Generators
    {state with Elevator = newFloor; Chips = newChips; Generators = newGens}

let isValid state = 
    state.Chips
    |> Seq.filter(fun kvp -> kvp.Value <> state.Generators.[kvp.Key])
    |> Seq.filter(fun kvp -> state.Generators |> Seq.filter(fun kv -> kv.Key <> kvp.Key && kv.Value = kvp.Value)|> Seq.isEmpty |> not)
    |> Seq.isEmpty
    
let adjacent state = 
    let chips = state.Chips |> Seq.filter(fun kvp -> kvp.Value = state.Elevator) |> Seq.map(fun x -> x.Key) |> Seq.toList
    let generators = state.Generators |> Seq.filter(fun kvp -> kvp.Value = state.Elevator) |> Seq.map(fun x -> x.Key) |> Seq.toList
    let candidateMoves =
        seq{
            yield!
                seq{for chip1 in chips do
                    for chip2 in chips do
                        yield [chip1; chip2] |> List.distinct |> List.sort, []}
            yield!
                seq{for gen1 in generators do
                    for gen2 in generators do
                        yield [], [gen1;gen2] |> List.distinct |> List.sort}
            yield!
                seq{for gen in generators do
                    for chip in chips do
                        yield [chip], [gen]}}
        |> Seq.distinct
        |> Seq.toList
    adjacentFloor state.Elevator
    |> List.collect(fun newFloor -> 
        candidateMoves 
        |> Seq.map(move newFloor state)
        |> Seq.toList)
    |> List.filter isValid

let heuristic state = 
    let chip_count = state.Chips |> Seq.sumBy(fun kvp -> match kvp.Value with | Fourth -> 0 | Third -> 1 | Second -> 2 | First -> 1)
    let gens_count = state.Generators |> Seq.sumBy(fun kvp -> match kvp.Value with | Fourth -> 0 | Third -> 1 | Second -> 2 | First -> 1)
    (chip_count + gens_count)/2 

let A_star startState = 
    let targetState = 
        {
            Elevator = Fourth
            Chips = startState.Chips  |> Map.map(fun k v -> Fourth)
            Generators = startState.Generators |> Map.map(fun k v -> Fourth)
        }
    let pathLengths = new System.Collections.Generic.Dictionary<Symbol,int>()
    let estimatedPathLengts = new System.Collections.Generic.Dictionary<Symbol,int>()
    pathLengths.Add(startState.Symbol,0)
    estimatedPathLengts.Add(startState.Symbol, heuristic startState)
    Seq.unfold(fun (frontier: Set<int*state>) -> 
        if frontier.IsEmpty then None else
        let dist,current = frontier.MinimumElement
        if current = targetState then
            Some(pathLengths.[current.Symbol], Set.empty)
        else
        let newFrontier =
            adjacent current
            |> List.fold(fun (front: Set<int*state>) neighbor ->
                let pathlength = 
                    if pathLengths.ContainsKey neighbor.Symbol then 
                        pathLengths.[neighbor.Symbol]
                    else
                        System.Int32.MaxValue - 1
                let newPathLength = pathLengths.[current.Symbol] + 1
                if newPathLength < pathlength then
                    let estimatedPathLength = 
                        if estimatedPathLengts.ContainsKey neighbor.Symbol then
                            estimatedPathLengts.[neighbor.Symbol]
                        else
                            -1
                    let newEstimatedPathLength = newPathLength + heuristic(neighbor)
                    pathLengths.[neighbor.Symbol]<- newPathLength
                    estimatedPathLengts.[neighbor.Symbol]<- newEstimatedPathLength
                    front.Remove(estimatedPathLength,neighbor).Add(newEstimatedPathLength,neighbor)
                else
                    front
                ) (frontier.Remove(dist,current))
        Some(pathLengths.[current.Symbol], newFrontier)
        ) (Set [0, startState])
    |> Seq.last

let part1 = 
    A_star initialState

let part2 = 
    let augmentation (m:Map<element,floor>) = m.Add(Dilithium, First).Add(Elerium, First)
    A_star {Elevator = First; Chips = augmentation initialState.Chips; Generators = augmentation initialState.Generators}