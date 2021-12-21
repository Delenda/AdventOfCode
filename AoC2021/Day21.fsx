let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day21.txt") 

let startPosition (line:string) =
    let m = System.Text.RegularExpressions.Regex.Match(line, "Player (\d) starting position: (\d)")
    let player = m.Groups.[1].Value |> int
    let position = m.Groups.[2].Value |> int
    (player,position)

let startpositions = 
    input
    |> Array.map startPosition
    |> Map

type gamestate = Map<int,int>*Map<int,int>*int

let initialGameState = (startpositions, Map [1,0;2,0],1)

let nextState ((position,score,turn):gamestate) dieRoll = 
    let newSq = 
        let tmp = position.[turn] + (dieRoll%10)
        if tmp > 10 then tmp - 10 else tmp
    let nextPosition = position.Add(turn,newSq)
    let nextScore = score.Add(turn, score.[turn] + newSq)
    let nextTurn = if turn = 1 then 2 else 1
    (nextPosition,nextScore,nextTurn)

let play = 
    (initialGameState,0)
    |> Seq.unfold(fun (((positions,score,turn) as state,die):gamestate*int) -> 
        let maxScore = max score.[1] score.[2]
        if maxScore >= 1000 then None else
        let dieRolls = [die+3;die+2;die+1] |> List.map(fun s -> if s > 100 then s - 100 else s)
        let newDie = dieRolls |> List.head
        let dieRoll = dieRolls |> List.sum
        let newGameState = nextState state dieRoll
        Some(newGameState,(newGameState,newDie)))

let part1 = 
    let idx, (_,scores,_) =  
        play 
        |> Seq.indexed
        |> Seq.last
    (min scores.[1] scores.[2]) * 3 * (idx+1)

let rec count ((position,score,turn) as state:gamestate) (counts:Map<gamestate,uint64*uint64>) =
    if score.[1] >=21 then (1UL,0UL),counts else
    if score.[2] >=21 then (0UL,1UL),counts else
    if counts.ContainsKey state then counts.[state],counts else
    let x,newCount = 
        seq {for roll1 in [1;2;3] do
             for roll2 in [1;2;3] do
             for roll3 in [1;2;3] do
             yield roll1 + roll2 + roll3}
        |> Seq.fold( fun ((c1,c2),cache) dieRoll->
            let newState = nextState state dieRoll
            let (count1,count2), newCache = count newState cache
            (c1+count1, c2+count2), newCache.Add(newState, (count1,count2))) ((0UL,0UL),counts)
    x, newCount.Add(state,x)

let part2 = 
    let (a,b),_ = count initialGameState Map.empty
    max a b