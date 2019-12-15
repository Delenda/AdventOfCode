let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day15.txt") 

#load "IntCode.fsx"

type position = {x:int; y:int}
type robotPosition = {x:int; y:int; progState:IntCode.programState; state:int64}

let initialMemory = (IntCode.createMemory input).Add(-1L,0L)
let program = (0L, initialMemory, [], [])

let move ((oldPos, oldMem, _, _):IntCode.programState) ((pos, inp ) : position*int64) =
    let newProgstate = (oldPos, oldMem, [], [inp])
    let (newPos, newMem, output, _) = Seq.unfold IntCode.programStep newProgstate |> Seq.last
    { x  = pos.x; y = pos.y; progState = (newPos, newMem, [],[]); state = output.Head}

let neighbor seen (pos:robotPosition) =
    [
        {x = pos.x + 1; y = pos.y}      , 1L
        {x = pos.x - 1; y = pos.y}      , 2L
        {x = pos.x;     y = pos.y + 1}  , 3L
        {x = pos.x;     y = pos.y - 1}  , 4L
    ] 
    |> List.filter(fun (a, _) -> seen |> Set.contains a |> not)
    |> List.map (move pos.progState)
    |> List.filter(fun rp -> rp.state <> 0L)
    |> Set

let bfs stopCondition ((frontier, seen, n, air): Set<robotPosition>*Set<position>*int*Set<robotPosition>) =
    if stopCondition air frontier then 
        None
    else
        let newFrontier =
            frontier 
            |> Seq.map (neighbor seen)
            |> Set.unionMany
        let newSeen = 
            Set.union seen (frontier |> Set.map(fun p -> {x = p.x; y = p.y}))
        let newAir = 
            newFrontier 
            |> Set.filter(fun pos -> pos.state = 2L)
        let output = n, newAir
        let newState = newFrontier, newSeen, n+1, newAir
        Some( output, newState)

let stopWhenAirSeen (air:Set<robotPosition>) (frontier:Set<robotPosition>) =
    air.IsEmpty |> not

let stopWhenFrontierEmpty (air:Set<robotPosition>) (frontier:Set<robotPosition>) =
    frontier.IsEmpty

let airPosition = 
    let initFrontier = 
        [{x = 0; y = 0; progState = program; state = 1L}]
        |> Set
    let air :Set<robotPosition>= Set.empty 
    Seq.unfold (bfs stopWhenAirSeen) (initFrontier,Set.empty, 0, air) |> Seq.last 

let part1 = 
        airPosition
        |> fst 
        |> (+) 1 //+1 because the air location was found in the frontier - we haven't actually moved there yet

let part2 = 
    let initPosition = airPosition |> snd
    Seq.unfold (bfs stopWhenFrontierEmpty) (initPosition,Set.empty, 0, Set.empty) |> Seq.last |> fst