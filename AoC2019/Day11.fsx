let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day11.txt") 

#load "IntCode.fsx"

type position = {x:int; y:int}

type direction = |Up|Down|Left|Right

type color = |Black|White

let step direction position : position= 
    match direction with
    | Up    -> {position with y = position.y + 1}
    | Down  -> {position with y = position.y - 1}
    | Left  -> {position with x = position.x - 1}
    | Right -> {position with x = position.x + 1}

let newDirection direction inp =
    match direction, inp with
    | Up,   0L -> Left
    | Up,   1L -> Right
    | Down, 0L -> Right
    | Down, 1L -> Left
    | Left, 0L -> Down
    | Left, 1L -> Up
    | Right,0L -> Up
    | Right,1L -> Down
    | _ -> failwith(sprintf "unexpected inp %d" inp)

let getColor (position:position) (panels:Map<position,color>) =
    if panels.ContainsKey position then
        panels.[position]
    else
        color.Black

let stepRobot ((direction, position, panels, state): direction*position*Map<position,color>*IntCode.programState) =
    let (initPos, initMem, _, _) = state
    if (IntCode.nextOpcode initPos initMem = IntCode.Halt) then
        None
    else
        let currentColor = getColor position panels
        let newInput = if currentColor = Black then 0L else 1L
        let (pos, mem, output, inp) as newProgState = Seq.unfold IntCode.programStep (initPos, initMem, [], [newInput]) |> Seq.last
        let newOutput = output |> List.last
        let newColor = if  newOutput = 0L then Black else if newOutput = 1L then White else failwith (sprintf "Invalid output %d" newOutput)
        let newDirection = newDirection direction (List.head output)
        let newPanels = panels.Add(position, newColor)
        let newPosition = step newDirection position
        
        let newState = (newDirection, newPosition, newPanels, (pos, mem, [], [])) 
        Some(panels,newState)
        
let paintPanels program initColor= 
    let initDirection = Up
    let initPosition = {x=0; y=0}
    let initPanels = [initPosition, initColor] |> Map
    let pos = 0L
    let mem = IntCode.createMemory program
    let initState = initDirection, initPosition, initPanels, (pos, mem.Add(-1L,0L), [], [])

    Seq.unfold stepRobot initState
    |> Seq.last

let part1 = 
    let panels = paintPanels input Black
    panels.Count

let part2 = 
    let panels = paintPanels input White
    let minY = panels |> Seq.map(fun k -> k.Key.y) |> Seq.min
    let maxY = panels |> Seq.map(fun k -> k.Key.y) |> Seq.max
    let minX = panels |> Seq.map(fun k -> k.Key.x) |> Seq.min
    let maxX = panels |> Seq.map(fun k -> k.Key.x) |> Seq.max
    seq{for i in [maxY..(-1)..minY] do
        let line = 
            seq{for j in [minX..maxX] do
                match getColor {x = j; y = i} panels with
                | Black -> yield " "
                | White -> yield "#"
                } |> String.concat ""
        yield line}
    |> Seq.iter(System.Console.WriteLine)    