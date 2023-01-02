let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day17.txt") 

type wind = |Right |Left

let wind = input.ToCharArray() |> Array.map(fun c -> if c = '<' then Left else Right)

let print (cave:Set<int*int>) = 
    let miny = cave |> Seq.map snd |> Seq.min
    let maxy = cave |> Seq.map snd |> Seq.max
    [maxy.. -1 ..miny]
        |> List.map(fun y -> [0..6] |> List.map(fun x -> if cave.Contains (x,y) then "#" else ".") |> String.concat"")
        |> List.iter System.Console.WriteLine
    System.Console.WriteLine ""

type tetris = 
    | Minus
    | Plus
    | Gamma
    | Stick
    | Ball
     
let shape = function
    | Minus -> Set [0,0;1,0;2,0;3,0] 
    | Plus -> Set [0,1; 1,0; 1,1; 1,2; 2,1] 
    | Gamma -> Set [0,2;1,2;2,0;2,1;2,2]
    | Stick ->  Set [0,0; 0,1;0,2;0,3]
    | Ball -> Set [0,0; 0,1; 1,0; 1,1]

let width = function 
    | Minus -> 4
    | Plus -> 3
    | Gamma -> 3
    | Stick -> 1
    | Ball -> 2

let height = function
    | Minus -> 1
    | Plus -> 3
    | Gamma -> 3
    | Stick -> 4
    | Ball -> 2

let nextShape = function
    | Minus -> Plus
    | Plus -> Gamma
    | Gamma -> Stick
    | Stick -> Ball
    | Ball -> Minus

let check (rock: tetris) (rocks:Set<int*int>) ((x,y):int*int) = 
    seq{ for (dx,dy) in (shape rock) do
         if rocks.Contains(x+dx, y-dy) then yield ()}
    |> Seq.isEmpty

let horizontalMove (wind:wind) (rock:tetris) (rocks: Set<int*int>) (pos:int*int) = 
    let (x,y) = pos
    let newX = 
        match wind with
        | Left -> max 0 (x-1)
        | Right -> min (x+1) (7 - width rock)
    if check rock rocks (newX,y) then 
        newX,y
    else 
        x,y

let verticalMove (rock:tetris) (rocks:Set<int*int>) (pos:int*int) = 
    let (x,y) = pos
    if check rock rocks (x, y - 1) then
        x, y-1
    else
        x,y

let dropRock maxheight (rocks:Set<int*int>) (windIdx:int) (rock:tetris) = 
    let ypos = height rock + maxheight + 3
    let position  = (2, ypos)
    let (restX,restY) = 
        Seq.initInfinite (fun i -> (windIdx+i)%wind.Length)
        |> Seq.scan(fun (rest,(x,y)) idx ->
            let wind = wind.[idx]
            if rest = true then (rest, (x,y)) else
            let (newX,newY) = 
                (x,y)
                |> horizontalMove wind rock rocks 
                |> verticalMove rock rocks
            if newY = y then 
                (true, (newX,y))
            else
                (false, (newX,newY))) (false, position)
        |> Seq.skipWhile(fst >> not)
        |> Seq.head
        |> snd
    let restShape = 
        seq{for (dx,dy) in shape rock do
              yield (restX + dx, restY - dy)}
        |> Set
    let newRocks = rocks + restShape
    let newIdx = (ypos - restY + 1 + windIdx)%wind.Length
    let newMaxHeight = max maxheight restY
    newRocks, newIdx, newMaxHeight

let floor = [0..6] |> List.map(fun d -> (d,0)) |> Set

let maxheight n =
    (floor, 0, Minus, 0)
    |> Seq.unfold(fun (rocks, windIdx, rock, maxheight) -> 
            let newRocks, newIdx, newMaxheight = dropRock maxheight rocks windIdx rock
            let newRock = nextShape rock
            let q = newRocks, newIdx, newRock, newMaxheight
            Some(newMaxheight,q))
    |> Seq.take n
    |> Seq.last

let (oldHeight,newHeight),(oldCount, newCount) = 
    (floor, 0, Minus, 0UL,0UL, Map[(floor, 0, Minus),(0UL,0UL)])
    |> Seq.unfold(fun ((floor, windIdx, rock, maxheight, count, seen): Set<int*int>*int*tetris*uint64*uint64*Map<Set<int*int>*int*tetris, uint64*uint64>) -> 
            if floor.IsEmpty then 
                None 
            else
                let relMaxHeight = floor |> Seq.map snd |> Seq.max
                let newRocks, newIdx, _ = dropRock relMaxHeight floor windIdx rock
                let newRock = nextShape rock
                let newFloor  = 
                    let height0 = newRocks |> Seq.groupBy fst |> Seq.map(fun (_,b) -> b |> Seq.map snd |> Seq.max) |> Seq.min
                    newRocks |> Set.filter(fun (a,b) -> b >= height0) |> Set.map(fun (a,b) -> a, b - height0)
                let newMaxHeight = maxheight + (newRocks |> Seq.map snd |> Seq.max |> uint64) - (relMaxHeight |> uint64)
                let newKey = newFloor, newIdx, newRock
                let newCount = count + 1UL
                if seen.ContainsKey newKey then 
                    let oldMaxHeight,oldCount = seen.[newKey]
                    let q = Set.empty, -1, Minus, 0UL, 0UL, Map.empty
                    Some(Some ((oldMaxHeight,newMaxHeight),(oldCount,newCount)), q)
                else
                    let newSeen = seen.Add(newKey, (newMaxHeight,newCount ))
                    let q = newFloor, newIdx, newRock, newMaxHeight,newCount, newSeen
                    Some(None,q))
    |> Seq.pick id
    
let part1 = maxheight 2022
let part2 = 
    let target = 1_000_000_000_000UL
    let period = newCount - oldCount
    let numberOfPeriods = (target-oldCount)/period
    let increasePrPeriod = newHeight - oldHeight
    let remainder = (target-oldCount)%period
    uint64(maxheight (int(oldCount + remainder))) + increasePrPeriod*numberOfPeriods