let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\Day15.txt")

type position =
    {Y:int;X:int}
    member this.Adjacent (cave : Set<position>) =
        [{X = this.X+1; Y = this.Y};{X = this.X-1; Y = this.Y};{X = this.X; Y = this.Y+1};{X = this.X; Y = this.Y-1}]
        |> Set
        |> Set.intersect cave

type species = | Elf | Goblin
type bandit =
    {Id: int; Species:species; HP : int; AttackPower:int; Position : position}
    member this.print =
        let s = match this.Species with | Elf -> "E" | Goblin -> "G"
        sprintf "%s (%d)" s (this.HP)

let parseCaveLine j (line:string) =
    line.ToCharArray()
    |> Array.mapi(fun i c -> match c with | '.' | 'G' | 'E' -> Some {X = i; Y = j} |_ -> None)
    |> Array.choose id

//let caveMap =
//    input
//    |> Array.indexed
//    |> Array.collect(fun (i,line) -> parseCaveLine i line)
//    |> Set.ofArray

let parseBandits attackpower j (line:string) =
    line.ToCharArray()
    |> Array.mapi(fun i c ->
                            match c with
                            |'E' -> Some {Id = 0; Species = Elf; HP = 200; AttackPower = attackpower; Position = {X = i; Y =j}}
                            |'G' -> Some {Id = 0; Species = Goblin; HP = 200; AttackPower = 3; Position = {X = i; Y =j}}
                            | _ -> None )
    |> Array.choose id

//let banditsStart =
//    input
//    |> Array.indexed
//    |> Array.collect(fun (i,line) -> parseBandits i line)
//    |> Array.toList


let connectedComponent (p:position) (graph:Set<position>) =
    let unfolder ((seen, frontier) : Set<position>*Set<position>) =
        if frontier.IsEmpty then
            None
        else
            let v = frontier |> Seq.head
            let newFrontier = (frontier.Remove v) |> Set.union (v.Adjacent (Set.difference graph seen))
            let newSeen = seen.Add v
            let q = newSeen, newFrontier
            Some(newSeen,q)
    Seq.unfold unfolder (Set.empty, Set.empty.Add p)
    |> Seq.last

type stepState = {Vertices : Set<position>; SmallestDistance : Map<position,int>; PreviousVertex : Map<position,position>; candidates : Set<position> }
let findSteps (p:position) (graph:Set<position>)  : stepState=
    let cp = connectedComponent p graph
    let initialVertexSet = cp
    let initialDistance = cp |> Set.map(fun v -> if v = p then (v,0) else (v,System.Int32.MaxValue)) |> Map.ofSeq
    let initialPredicessors = Map.empty
    let initialState = {Vertices = initialVertexSet; SmallestDistance = initialDistance; PreviousVertex = initialPredicessors; candidates = Set.empty.Add p}
    let unfolder (s:stepState) =
        if s.Vertices.IsEmpty then
            None
        else
            let u = s.candidates |> Set.toSeq |> Seq.sortBy(fun v -> s.SmallestDistance.[v]) |> Seq.head
            let newVertices = s.Vertices.Remove u
            let neighbours = u.Adjacent newVertices
            let newCandidates = (Set.union s.candidates neighbours).Remove u
            let distU = s.SmallestDistance.[u]
            let update ((dist,prev) : Map<position,int>*Map<position,position>) (v:position)  =
                let alternateDistance = distU + 1
                if alternateDistance < dist.[v] then
                    let di = dist.Remove v
                    let pr = prev.Remove v
                    di.Add(v, alternateDistance), pr.Add(v,u)
                else
                    dist,prev
            let newDistance , newPrevious = neighbours |> Set.fold update (s.SmallestDistance, s.PreviousVertex)
            let q = {Vertices = newVertices; SmallestDistance = newDistance; PreviousVertex = newPrevious; candidates = newCandidates}
            Some(q,q)
    Seq.unfold unfolder initialState |> Seq.last

//let findPath (source:position) (target:position) =
//    let distanceMap = findSteps source caveMap
//    let rec constructPaths (s:position) : position list=
//        if s = source then
//            [s]
//        else
//            [s] @ ( constructPaths distanceMap.PreviousVertex.[s])
//    constructPaths target

//let findShortestDistance (source:position) (target:position) graph=
//    let distanceMap = findSteps source graph
//    distanceMap.SmallestDistance.[target]



let debug = false


let tick cm (bandits : bandit list) =
    let order =
        bandits
        |> List.sortBy(fun b -> b.Position)
    let unfoldBattle ((moved, waiting) : (bandit list) * (bandit list)) =
        if waiting.IsEmpty then
            None
        else
            let newId = waiting.Head.Id + 1
            let bandit= {waiting.Head with Id = newId }
            if debug then
                System.Console.WriteLine(sprintf "Bandit %A" bandit.Position )
            else
                ()
            let other = moved @ (waiting.Tail)
            let positions =
                other |> List.map(fun b -> b.Position) |> Set
            let adjacent = bandit.Position.Adjacent cm
            let enemies =
                other
                |> List.filter(fun b -> b.Species <> bandit.Species)
            let enemyPositions=
                enemies
                |> List.map(fun b -> b.Position)
                |> List.sort
                |> Set

            if (enemyPositions |> Set.intersect adjacent).IsEmpty then
                //Move
                if debug then
                    System.Console.WriteLine("Move")
                else
                    ()
                let graph = connectedComponent bandit.Position (Set.difference cm positions)
                let accesibleEnemies =
                    enemyPositions
                    |> Seq.map(fun p -> p.Adjacent graph)
                    |> Set.unionMany
                    //|> Set.intersect graph
                if accesibleEnemies.IsEmpty then
                    if debug then
                        System.Console.WriteLine ("No enemies")
                    else
                        ()
                    let newWaiting = waiting.Tail
                    let newMoved = bandit::moved
                    let q = (newMoved, newWaiting)
                    Some(newMoved@newWaiting |> Set,q)
                else
                    let possibleSteps =
                        bandit.Position.Adjacent graph
                    if possibleSteps.IsEmpty then
                        if debug then
                            System.Console.WriteLine "Surrounded"
                        else
                            ()
                        let newWaiting = waiting.Tail
                        let newMoved = bandit::moved
                        let q = (newMoved, newWaiting)
                        Some(newMoved@newWaiting |> Set,q)
                    else
                        if debug then
                            System.Console.WriteLine "Calculate distance"
                        else
                            ()
                        let (dist,target) =
                            let distanceMap = findSteps bandit.Position graph
                            accesibleEnemies
                            |> Seq.groupBy(fun b -> distanceMap.SmallestDistance.[b])
                            |> Seq.minBy fst
                            |> fun (x,y) -> (x, y |> Seq.min)
                        if debug then
                            System.Console.WriteLine "Finding next step"
                        else
                            ()
                        let newpos =
                            let distanceMap2 = findSteps target graph
                            possibleSteps
                            |> Seq.filter(fun p -> distanceMap2.SmallestDistance.[p] = dist - 1)
                            |> Seq.min
                        let newBandit = {bandit with Position = newpos}
                        let newWaiting = waiting.Tail
                        let newMoved = newBandit::moved
                        if dist = 1 then
                            if debug then
                                System.Console.WriteLine "Finding enemies"
                            else
                                ()
                            let newAdjacent = newBandit.Position.Adjacent cm
                            let adjacentEnemies =
                                other
                                |> List.filter(fun b -> b.Species <> newBandit.Species)
                                |> List.filter(fun b -> Set.contains b.Position newAdjacent)
                            let enemy=
                                adjacentEnemies
                                |> List.groupBy(fun b -> b.HP)
                                |> List.minBy fst
                                |> snd
                                |> List.minBy(fun b -> b.Position)
                            if debug then
                                System.Console.WriteLine "Attack"
                            else
                                ()
                            let enemyPosition = enemy.Position
                            let enemy = other |> List.filter(fun b -> b.Position = enemyPosition) |> List.head
                            let newEnemyHP = enemy.HP - bandit.AttackPower
                            if  newEnemyHP <= 0 then
                                let newNewMoved = (newMoved |> List.filter(fun b -> b.Position <> enemyPosition))
                                let newNewWaiting = (newWaiting |> List.filter(fun b -> b.Position <> enemyPosition))
                                let q = (newNewMoved, newNewWaiting)
                                Some(newNewMoved@newNewWaiting |> Set,q)
                            else
                                let newEnemy = {enemy with HP = enemy.HP - bandit.AttackPower}
                                let newNewMoved = (newMoved |> List.map(fun b -> if b.Position <> enemyPosition then b else newEnemy))
                                let newNewWaiting = newWaiting|> List.map(fun b -> if b.Position <> enemyPosition then b else newEnemy)
                                let q = (newNewMoved, newNewWaiting)
                                Some(newNewMoved@newNewWaiting |> Set,q)
                        else
                            let q = (newMoved, newWaiting)
                            Some(newMoved@newWaiting |> Set,q)

            else
                //Fight
                if debug then
                    System.Console.WriteLine("Fight")
                    System.Console.WriteLine "Finding enemies"
                else
                    ()
                let adjacentEnemies =
                    other
                    |> List.filter(fun b -> b.Species <> bandit.Species)
                    |> List.filter(fun b -> Set.contains b.Position adjacent)
                let enemy=
                    adjacentEnemies
                    |> List.groupBy(fun b -> b.HP)
                    |> List.minBy fst
                    |> snd
                    |> List.minBy(fun b -> b.Position)
                if debug then
                    System.Console.WriteLine "Attack"
                else
                    ()
                let enemyPosition = enemy.Position
                let newEnemyHP = enemy.HP - bandit.AttackPower
                if  newEnemyHP <= 0 then
                    let newMoved = bandit::(moved |> List.filter(fun b -> b.Position <> enemyPosition))
                    let newWaiting = (waiting.Tail |> List.filter(fun b -> b.Position <> enemyPosition))
                    let q = (newMoved, newWaiting)
                    Some(newMoved@newWaiting |> Set,q)
                else
                    let newEnemy = {enemy with HP = enemy.HP - bandit.AttackPower}
                    let newMoved = bandit::(moved |> List.map(fun b -> if b.Position <> enemyPosition then b else newEnemy))
                    let newWaiting = waiting.Tail|> List.map(fun b -> if b.Position <> enemyPosition then b else newEnemy)
                    let q = (newMoved, newWaiting)
                    Some(newMoved@newWaiting |> Set,q)
    Seq.unfold unfoldBattle ([], order)

//let testBattle =
//    [|
//        "#######"
//        "#.G...#"
//        "#...EG#"
//        "#.#.#G#"
//        "#..G#E#"
//        "#.....#"
//        "#######"
//    |]

//let testCaveMap =
//    testBattle
//    |> Array.indexed
//    |> Array.collect(fun (i,line) -> parseCaveLine i line)
//    |> Set.ofArray

//let testBandits =
//    testBattle
//    |> Array.indexed
//    |> Array.collect(fun (i,line) -> parseBandits i line)
//    |> Array.toList

let checksum attackpower (caveStr : string array) =
    let cavemap =
        caveStr
        |> Array.indexed
        |> Array.collect(fun (i,line) -> parseCaveLine i line)
        |> Set.ofArray

    let bandits =
        caveStr
        |> Array.indexed
        |> Array.collect(fun (i,line) -> parseBandits attackpower i line)
        |> Array.toList

    let print (s:bandit list) =
        let round = s.Head.Id
        let m =
            s
            |> List.fold(fun t b ->
                    t
                    |> Array.mapi(fun i (g:string)->
                                    if i <> b.Position.Y then
                                        g
                                    else
                                        let c = match b.Species with | Elf -> "E" | Goblin -> "G"
                                        let h : string = g.Remove(b.Position.X,1)
                                        h.Insert(b.Position.X,c)
                            ) ) (caveStr |> Array.map(fun g -> g.Replace("G",".").Replace("E",".")))
        let bStr =
            s
            |> List.groupBy (fun b -> b.Position.Y)
            |> List.map(fun (x,y) -> (x, y|> List.sortBy(fun b-> b.Position) |> List.map(fun b -> b.print)|> Array.ofList|> String.concat ", "  ))
            |> Map
        System.Console.WriteLine(sprintf "*** Round %d ***" round)
        m
        |> Array.mapi(fun i g ->
                            if bStr.ContainsKey i then
                                g+"   "+(bStr.[i])
                            else
                                g
                                )
        |> Array.iter(fun g -> System.Console.WriteLine g)


    let victory =
        Seq.unfold (fun s ->
                        let elfs = s |> List.filter(fun b -> b.Species = Elf) |> List.length
                        let goblins = s |> List.filter(fun b -> b.Species = Goblin) |> List.length
                        if elfs = 0 || goblins = 0 then
                            None
                        else
                            print s
                            let sq = tick cavemap s
                            Some(sq, sq |> Seq.last |> Set.toList)

                                    )  bandits
        |> Seq.collect id
        |> Seq.last
        |> Seq.sortBy(fun b -> b.Position)
        |> Seq.toList

    let hpsum = victory |> Seq.sumBy(fun b -> b.HP)
    let rounds = victory |> Seq.map(fun b -> b.Id) |> Seq.min
    hpsum * (rounds-1)

//checksum testBattle

//let test2 =
//    [|
//        "#######"
//        "#G..#E#"
//        "#E#E.E#"
//        "#G.##.#"
//        "#...#E#"
//        "#...E.#"
//        "#######"
//    |]

//checksum test2

//let test3  =
//    [|
//        "#######"
//        "#E..EG#"
//        "#.#G.E#"
//        "#E.##E#"
//        "#G..#.#"
//        "#..E#.#"
//        "#######"
//    |]

//checksum test3

//let test4 =
//    [|
//        "#######"
//        "#E.G#.#"
//        "#.#G..#"
//        "#G.#.G#"
//        "#G..#.#"
//        "#...E.#"
//        "#######"
//    |]
//checksum test4

//let test5 =
//    [|
//        "#######"
//        "#.E...#"
//        "#.#..G#"
//        "#.###.#"
//        "#E#G#G#"
//        "#...#G#"
//        "#######"
//        |]
//checksum test5

//let test6 =
//    [|
//        "#########"
//        "#G......#"
//        "#.E.#...#"
//        "#..##..G#"
//        "#...##..#"
//        "#...#...#"
//        "#.G...G.#"
//        "#.....G.#"
//        "#########"
//|]
//checksum test6

let part1 =
    checksum 3 input


let part2 =
    checksum 20 input
