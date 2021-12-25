let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day23.txt") 

type state = 
    {
        Feed : Map<char, (char*int) list>
        Hall : Map<int,char>
        Arrived:Map<char,int>
    }

let cost = function
    | 'A' -> 1
    | 'B' -> 10
    | 'C' -> 100
    | 'D' -> 1000
    | _ -> "unexpected char" |> failwith

let parse (lines: string array) =
    let rooms2 = lines.[3].ToCharArray()
    let rooms1 = lines.[2].ToCharArray()
    {Hall = Map.empty
     Arrived = ['A',0;'B',0;'C',0;'D',0] |> Map
     Feed =  Map ['A', [rooms1.[3], 2 * cost rooms1.[3];rooms2.[3], 3 * cost rooms2.[3]]
                  'B', [rooms1.[5], 2 * cost rooms1.[5];rooms2.[5], 3 * cost rooms2.[5]]
                  'C', [rooms1.[7], 2 * cost rooms1.[7];rooms2.[7], 3 * cost rooms2.[7]]
                  'D', [rooms1.[9], 2 * cost rooms1.[9];rooms2.[9], 3 * cost rooms2.[9]]]}

let destinationState = 
    {Hall = Map.empty
     Arrived = ['A',2;'B',2;'C',2;'D',2] |> Map
     Feed =  Map ['A', []
                  'B', []
                  'C', []
                  'D', []]}

let collectAlreadyArrived (state:state) = 
    let newFeed = 
        state.Feed
        |> Map.map(fun room feed -> feed |> List.rev |> List.skipWhile(fun (amph,_) -> amph = room) |> List.rev )
    let newArrived = 
        state.Arrived |> Map.map(fun room _ -> state.Feed.[room].Length - newFeed.[room].Length)
    {state with Feed = newFeed; Arrived = newArrived}

let pathsToHall = 
        [
            'A', Map [2, [2]
                      1, [2;1]
                      3, [3]
                      4, [3;4]
                      5, [3;4;5]
                      6, [3;4;5;6]
                      7, [3;4;5;6;7]]
            'B', Map [3, [3]
                      2, [3;2]
                      1, [3;2;1]
                      4, [4]
                      5, [4;5]
                      6, [4;5;6]
                      7, [4;5;6;7]]
            'C', Map [4, [4]
                      3, [4;3]
                      2, [4;3;2]
                      1, [4;3;2;1]
                      5, [5]
                      6, [5;6]
                      7, [5;6;7]
                      5, [5]]
            'D', Map [5, [5]
                      4, [5;4]
                      3, [5;4;3]
                      2, [5;4;3;2]
                      1, [5;4;3;2;1]
                      6, [6]
                      7, [6;7]]
        ] |> Map

let stepsize = function
    | [2;3] | [3;2] 
    | [3;4] | [4;3]
    | [4;5] | [5;4]
    | [5;6] | [6;5] -> 2
    | _ -> 1

let stepCountToHall = 
    pathsToHall
    |> Map.map(fun room paths -> paths |> Map.map(fun room path -> path |> List.windowed 2 |> List.sumBy stepsize) )

let pathsFromHall = 
    pathsToHall
    |> Map.toSeq
    |> Seq.collect(fun (x,y) -> y |> Map.toSeq |> Seq.map(fun (z,w) -> x,z,w))
    |> Seq.groupBy(fun (room, hall, path) -> hall)
    |> Seq.map(fun (hall, x) -> hall,x |> Seq.map(fun (room,_,path) -> room,path |> List.rev) |> Map)
    |> Map

let makeToHallState (state:state) room amph initialCost hall= 
    let stepcount =  (stepCountToHall.[room].[hall])
    let newFeed = state.Feed |> Map.map(fun r feed -> if r = room then feed.Tail else feed)
    let newHall = state.Hall.Add(hall, amph)
    let totalCost = initialCost + (cost amph) * stepcount 
    {state with Feed = newFeed; Hall = newHall}, totalCost

let nextToHallStates (state:state) =
    state.Feed
    |> Map.filter(fun _ feed -> feed.IsEmpty |> not)
    |> Map.map(fun room feed -> feed.Head)
    |> Map.toSeq
    |> Seq.collect(fun (room, (amph,cost)) -> pathsToHall.[room] |> Map.toSeq |> Seq.map(fun (x,y) -> room,amph,cost,x,y))
    |> Seq.filter(fun (_,_,_,_,path) -> path |> List.forall(fun i -> state.Hall.ContainsKey i |> not))
    |> Seq.map (fun (room,amph, cost, hall, _) -> makeToHallState state room amph cost hall)

let makeFromHallState feedsize (state:state) hall amph  =
    let hall_stepcount = stepCountToHall.[amph].[hall] + 2
    let room_stepcount = feedsize - state.Arrived.[amph] - 1
    let newHall = state.Hall.Remove hall
    let newArrived = state.Arrived.Add(amph, state.Arrived.[amph] + 1)
    let totalCost = (hall_stepcount + room_stepcount) * (cost amph)
    {state with Hall = newHall; Arrived = newArrived}, totalCost

let nextFromHallStates feedsize (state:state) =
    state.Hall
    |> Map.toSeq
    |> Seq.filter(fun (hall,amph) -> state.Feed.[amph] |> List.isEmpty)
    |> Seq.filter(fun (hall,amph) -> pathsFromHall.[hall].[amph] |> List.tail |> List.forall(fun i -> state.Hall.ContainsKey i |> not))
    |> Seq.map (fun (hall, amph) -> makeFromHallState feedsize state hall amph)

let nextStates feedsize (state:state) = Seq.append (nextToHallStates state) (nextFromHallStates feedsize state)

let heuristic (state:state) = 
    let hall_dists = state.Hall |> Map.toSeq |> Seq.sumBy(fun (hall,amph) -> stepCountToHall.[amph].[hall] * cost amph)
    let feed_dists = state.Feed |> Map.toSeq |> Seq.sumBy(fun (room, feed) -> feed |> List.sumBy snd)
    hall_dists + feed_dists

let A_star feedsize startState targetState= 
    let pathLengths = new System.Collections.Generic.Dictionary<state,int>()
    let estimatedPathLengts = new System.Collections.Generic.Dictionary<state,int>()
    pathLengths.Add(startState,0)
    estimatedPathLengts.Add(startState, heuristic startState)
    Seq.unfold(fun (frontier: Set<int*state>) -> 
        if frontier.IsEmpty then None else
        let dist,current = frontier.MinimumElement
        if current = targetState then
            Some(pathLengths.[current], Set.empty)
        else
        let newFrontier =
            nextStates feedsize current
            |> Seq.fold(fun (front: Set<int*state>) (neighbor,cost) ->
                let pathlength = 
                    if pathLengths.ContainsKey neighbor then 
                        pathLengths.[neighbor]
                    else
                        pathLengths.[current] + cost + 1
                let newPathLength = pathLengths.[current] + cost
                if newPathLength < pathlength then
                    let estimatedPathLength = 
                        if estimatedPathLengts.ContainsKey neighbor then
                            estimatedPathLengts.[neighbor]
                        else
                            -1
                    let newEstimatedPathLength = newPathLength + heuristic(neighbor)
                    pathLengths.[neighbor]<- newPathLength
                    estimatedPathLengts.[neighbor]<- newEstimatedPathLength
                    front.Remove(estimatedPathLength,neighbor).Add(newEstimatedPathLength,neighbor)
                else
                    front
                ) (frontier.Remove(dist,current))
        Some(pathLengths.[current], newFrontier)
        ) (Set [0, startState])
    |> Seq.last

let newFeed feed insert  =
    let oldAmphs = feed |> List.map fst
    let newAmphs = oldAmphs.Head::insert@oldAmphs.Tail
    newAmphs 
    |> List.indexed 
    |> List.map(fun (i, amph) -> 
        let steps = 2 + i
        let initialCost = cost amph * steps
        amph,initialCost)

let parse_expanded input= 
    let init = parse input
    let insert = 
        ['A', ['D';'D']
         'B', ['C';'B']
         'C', ['B';'A']
         'D', ['A';'C']] |> Map
    let expandedFeed = init.Feed |> Map.map(fun room feed -> newFeed feed insert.[room])
    {init with Feed = expandedFeed} |> collectAlreadyArrived

let initState = parse input |> collectAlreadyArrived
let initState2 = parse_expanded input
let destinationState2 = {destinationState with Arrived = destinationState.Arrived |> Map.map(fun _ _ -> 4)}

let part1 = A_star 2 initState destinationState
let part2 = A_star 4 initState2 destinationState2