let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day22.txt")

let coords = input |> Array.mapi(fun i s -> s |> Seq.mapi(fun j c -> match c with | '#' -> Some (i-12,j-12) | _ -> None ) |> Seq.toArray |> Array.choose id) |> Array.collect id

type state = {pos : int*int; direction : int*int; count:int; infected : (int*int) Set}

let init = {pos = (0,0); direction = (-1,0); count = 0; infected = coords |> Set.ofArray}

let step state =
    let infection = state.infected.Contains state.pos
    let newDirection = 
        match infection, state.direction with
        | true, (a,b) -> (b,-a)
        | false, (a,b) -> (-b,a) 
    let newCount =
        match infection with
        | true -> state.count
        | false -> state.count + 1
    let newPos = (fst state.pos + fst newDirection, snd state.pos + snd newDirection)
    let newInfected =
        match infection with
        | true -> state.infected.Remove state.pos
        | false -> state.infected.Add state.pos
    let newState = {pos = newPos; direction = newDirection; count = newCount; infected = newInfected}
    Some(newState,newState)

let question1 =
    Seq.unfold step init |> Seq.take 10000 |> Seq.last |> fun s-> s.count

type condition = | Clean | Weakened | Flagged | Infected

type state2 =  {pos: int*int; direction :int*int; count:int; infected : (int*int) Set; weakened : (int*int) Set; flagged : (int*int) Set}

let init2 = {pos = (0,0); direction=(-1,0); count = 0; infected = coords |> Set.ofArray ; weakened = Set.empty; flagged = Set.empty }

let getCondition state =
    if state.infected.Contains state.pos then
        condition.Infected
    else if state.weakened.Contains state.pos then
        condition.Weakened
    else if state.flagged.Contains state.pos then
        condition.Flagged
    else
        condition.Clean

let step2 state = 
    let condition = getCondition state
    let newDirection = 
        match condition, state.direction with
        | condition.Clean, (a,b) -> (-b,a)
        | condition.Weakened, (a,b) -> (a,b)
        | condition.Infected, (a,b) -> (b,-a)
        | condition.Flagged, (a,b) -> (-a,-b) 
    let newCount =
        match condition with
        | condition.Clean -> state.count
        | condition.Weakened -> state.count + 1
        | condition.Infected -> state.count
        | condition.Flagged -> state.count
    let newPos = (fst state.pos + fst newDirection, snd state.pos + snd newDirection)
    let newInfected, newWeakened, newFlagged =
        match condition with
        | condition.Clean       -> state.infected, state.weakened.Add state.pos, state.flagged
        | condition.Weakened    -> state.infected.Add state.pos, state.weakened.Remove state.pos, state.flagged
        | condition.Infected    -> state.infected.Remove state.pos, state.weakened, state.flagged.Add state.pos
        | condition.Flagged     -> state.infected, state.weakened, state.flagged.Remove state.pos
    let newState = {pos = newPos; direction = newDirection; count = newCount; infected = newInfected; weakened = newWeakened; flagged = newFlagged}
    Some(newState,newState)

let question2 = 
    Seq.unfold step2 init2 |> Seq.take 10000000 |> Seq.last |> fun s -> s.count