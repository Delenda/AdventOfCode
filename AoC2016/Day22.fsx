let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day22.txt") 

type position = 
    {X: int; Y:int}

type node = 
    { 
        Size : int
        Used : int
    }
    member this.Avail = 
        this.Size - this.Used

let parse (line:string) =
    let m = System.Text.RegularExpressions.Regex.Match(line, @"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%")
    let getInt (idx:int)= m.Groups.[idx].Value |> int
    {X = getInt 1; Y = getInt 2},{Size = getInt 3; Used = getInt 4}

let nodes = 
    input 
    |> Seq.skip 2
    |> Seq.map parse
    |> Map
    |> fun m -> m.Add({X=0;Y=0}, {Size = 100; Used = 1})

let dist a b = abs(a.X - b.X) + abs(a.Y - b.Y)

type Symbol = position*position
type state = 
    {
        Goal : position
        Free : position
        Used : Map<position,int>
    }
    member this.Symbol : Symbol = this.Goal, this.Free

let goal = nodes |> Map.toSeq |> Seq.map fst |> Seq.filter(fun x -> x.Y = 0) |> Seq.maxBy(fun x -> x.X)
let free = nodes |> Map.filter(fun k v -> v.Used = 0) |> Map.toSeq |> Seq.map fst |> Seq.exactlyOne
let target1 = {Goal = {X = 0; Y=0}; Free = {X = 0; Y = 1}; Used = Map.empty}
let target2 = {Goal = {X = 0; Y=0}; Free = {X = 1; Y = 0}; Used = Map.empty}

let heuristic state = 
    if state.Goal = target1.Goal then 0 else
    dist state.Free {state.Goal with X = state.Goal.X - 1} + state.Goal.X + state.Goal.Y

let adjacent state = 
    let free = state.Free
    [
        {free with X = free.X - 1}
        {free with X = free.X + 1}
        {free with Y = free.Y - 1}
        {free with Y = free.Y + 1}
    ]
    |> List.filter(nodes.ContainsKey)
    |> List.filter(fun x -> state.Used.[x] <= nodes.[free].Size )
    |> List.map(fun x -> 
          {state with Free = x; Used = state.Used.Add(free, state.Used.[x]).Add(x, 0); Goal = if x = state.Goal then free else state.Goal} )
    
let A_star (startState:state) = 
    let pathLengths = new System.Collections.Generic.Dictionary<Symbol,int>()
    let estimatedPathLengts = new System.Collections.Generic.Dictionary<Symbol,int>()
    pathLengths.Add(startState.Symbol,0)
    estimatedPathLengts.Add(startState.Symbol, heuristic startState)
    Seq.unfold(fun (frontier: Set<position*int*state>) -> 
        if frontier.IsEmpty then None else
        let goal,dist,current = frontier.MinimumElement
        if current.Symbol = target1.Symbol || current.Symbol = target2.Symbol then
            Some(pathLengths.[current.Symbol], Set.empty)
        else
        let newFrontier =
            adjacent current
            |> List.fold(fun (front: Set<position*int*state>) neighbor ->
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
                    front.Remove(goal,estimatedPathLength,neighbor).Add(neighbor.Goal,newEstimatedPathLength,neighbor)
                else
                    front
                ) (frontier.Remove(goal,dist,current))
        Some(pathLengths.[current.Symbol], newFrontier)
        ) (Set [startState.Goal,heuristic startState, startState])
    |> Seq.last

let part1 = 
    seq{for nodeA in nodes do
        for nodeB in nodes do
            if nodeA.Key <> nodeB.Key && nodeA.Value.Used <> 0 && nodeA.Value.Used <= nodeB.Value.Avail then 
                yield nodeA}
    |> Seq.distinct
    |> Seq.length

let part2 = 
    A_star {Free = free; Goal = goal; Used = nodes |> Map.map(fun k v -> v.Used)}