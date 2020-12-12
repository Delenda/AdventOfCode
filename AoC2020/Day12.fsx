let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day12.txt")

type direction =
    | North
    | East
    | South
    | West

type instruction =
    | Move_North of int
    | Move_South of int
    | Move_East of int
    | Move_West of int
    | Turn of int
    | Forward of int

let turn direction degree = 
    match direction, degree with
    | North, 90
    | West, 180
    | South, 270 -> East
    | West, 90
    | South, 180
    | East, 270 -> North
    | South, 90
    | East, 180
    | North, 270 -> West
    | East, 90
    | North, 180
    | West, 270 -> South
    | _ -> failwith (sprintf "Unexpected turn: %A, %d" direction degree)

let turn_waypoint (x,y) degree = 
    match degree with
    | 90 -> (y,-x)
    | 180 -> (-x, -y)
    | 270 -> (-y, x)
    | _ -> failwith (sprintf "Unexpected degreed: %d" degree)

let parse txt = 
    let pattern = @"([N|S|E|W|L|R|F])(\d+)"
    seq{for m in System.Text.RegularExpressions.Regex.Matches(txt, pattern) do
         let instr = m.Groups.[1].Value
         let value = m.Groups.[2].Value |> int
         yield 
             match instr with
             | "N" -> Move_North value
             | "S" -> Move_South value
             | "E" -> Move_East value
             | "W" -> Move_West value
             | "L" -> Turn (360-value)
             | "R" -> Turn value
             | "F" -> Forward value
             | _ -> failwith (sprintf "Unexpected instruction: %s " instr)}

type state = {X: int; Y : int; Direction : direction; Waypoint_X : int; Waypoint_Y : int }    

let rec move state instruction = 
    match instruction with
    | Move_North steps -> {state with Y = state.Y + steps}
    | Move_South steps -> {state with Y = state.Y - steps}
    | Move_East steps -> {state with X = state.X + steps}
    | Move_West steps -> {state with X = state.X - steps}
    | Turn deg -> {state with Direction = turn state.Direction deg}
    | Forward steps ->
        match state.Direction with
        | North -> move state (Move_North steps) 
        | South -> move state (Move_South steps) 
        | West -> move state (Move_West steps) 
        | East -> move state (Move_East steps) 

let rec move2 state instruction = 
    match instruction with
    | Move_North steps -> {state with Waypoint_Y = state.Waypoint_Y + steps}
    | Move_South steps -> {state with Waypoint_Y = state.Waypoint_Y - steps}
    | Move_East steps -> {state with Waypoint_X = state.Waypoint_X + steps}
    | Move_West steps -> {state with Waypoint_X = state.Waypoint_X - steps}
    | Turn deg -> 
        let newWaypoint = turn_waypoint (state.Waypoint_X, state.Waypoint_Y) deg
        {state with Waypoint_X = fst newWaypoint; Waypoint_Y = snd newWaypoint}
    | Forward steps -> {state with X = state.X + steps* state.Waypoint_X; Y = state.Y + steps * state.Waypoint_Y}
        
let initialState = {X = 0; Y = 0; Direction = East; Waypoint_X = 10; Waypoint_Y = 1}

let part1 = 
    input
    |> parse
    |> Seq.fold move initialState
    |> fun s -> abs(s.X) + abs(s.Y)

let part2 = 
    input
    |> parse
    |> Seq.fold move2 initialState
    |> fun s -> abs(s.X) + abs(s.Y)