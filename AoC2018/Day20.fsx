let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\input\Day20.txt")

type symbol =
    | Begin
    | End
    | Bifurcation
    | Fork
    | Loop
    | Option
    | North
    | South
    | East
    | West

type increments = 
    | Xup
    | Xdown
    | Yup
    | Ydown

let symbolToInc = 
    function
    | North -> Yup
    | South -> Ydown
    | East -> Xup
    | West -> Xdown
    | _ -> failwith "Unexpected direction"

let charToSymbol (c:char) : symbol = 
    match c with
    | '^' -> Begin
    | '$' -> End
    | '(' -> Bifurcation
    | ';' -> Loop
    | ')' -> Fork
    | 'N' -> North
    | 'S' -> South
    | 'E' -> East
    | 'W' -> West
    | '|' -> Option
    | _ -> failwith(sprintf "Uventet character: %s" (string c))

let lex (regex: string) : symbol list =
    regex.Replace("|)",";").ToCharArray() |> Array.map charToSymbol |> List.ofArray

let test1 = "^ENWWW(NEEE|SSE(EE|N))$"
let test2 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
let test3 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
let test4 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

let test1Symbols = lex test1
let test2Symbols = lex test2
let test3Symbols = lex test3
let test4Symbols = lex test4
let problemSymbols = lex input

let (|Direction|Forking|Border|) s =
    match s with
    | North | South | East | West -> Direction
    | Bifurcation | Fork | Loop | Option -> Forking
    | Begin | End -> Border

let rec slurpDirections s = 
    match s with
    | [] -> [],s
    | x::xs -> 
        match x with
        | Direction ->
            let a, b = slurpDirections xs 
            symbolToInc x :: a, b
        | Forking | Border -> [],s

let rec parse s =
    match s with
    | [] -> [],[]
    | x::xs -> 
        match x with
        | Begin -> parse xs
        | End -> [],[]
        | Direction -> 
            let a, b = slurpDirections s
            let c,d = parse b
            if c.IsEmpty then
                [a],d
            else
                c |> List.map(fun l -> a@l), d
        | Bifurcation -> 
            let a,b = parseBifurcation s
            let c,d = parse (b.Tail)
            if c.IsEmpty then
                a,d
            else
                let bifurcationType = (b.Head)
                match bifurcationType with
                | Fork -> c |> List.collect(fun l -> a |> List.map(fun k -> k@l)), d
                | Loop -> a@c, d
                | _ -> failwith (sprintf "Uventet bifurcation %A" bifurcationType)
        | Forking -> [], s
        | _ -> failwith (sprintf "Uventet symbol: %A" x)
and parseBifurcation s : (increments list list * symbol list)=
    match s with
    | [] -> [],s
    | x::xs -> 
        match x with
        | Bifurcation | Option -> 
            let a, b = parse xs
            let c,d = parseBifurcation b
            let e = a@c
            e,d
        | Forking -> [],s
        | Border  | Direction -> failwith (sprintf "Uventet bifurcation symbol %A" x)

let roomCoordinates s t = 
    match t with
    | Yup -> fst s, (snd s) + 1
    | Ydown -> fst s, (snd s) - 1
    | Xup -> (fst s) + 1, snd s
    | Xdown -> (fst s) - 1, snd s

let paths, rest = parse problemSymbols

let coordinates  =
    paths
    |> List.collect (List.scan roomCoordinates (0,0))
    |> List.distinct
    |> Set

let links = 
    paths
    |> List.map (List.scan roomCoordinates (0,0))
    |> List.collect (List.windowed 2)
    |> List.collect (fun s -> [(s.Head,s.Tail.Head);(s.Tail.Head,s.Head)])
    |> List.distinct    
    |> List.groupBy fst
    |> List.map(fun (x,y) -> (x, y |> List.map snd |> Set))
    |> Map

type stepState = {Vertices : Set<int*int>; SmallestDistance : Map<int*int,int>; PreviousVertex : Map<int*int,int*int> }
let findSteps (p:int*int) : stepState=
    let initialVertexSet = coordinates
    let initialDistance = coordinates |> Set.map(fun v -> if v = p then (v,0) else (v,System.Int32.MaxValue)) |> Map.ofSeq
    let initialPredicessors = Map.empty
    let initialState = {Vertices = initialVertexSet; SmallestDistance = initialDistance; PreviousVertex = initialPredicessors}
    let unfolder (s:stepState) =
        if s.Vertices.IsEmpty then
            None
        else
            let u = s.Vertices |> Set.toSeq |> Seq.sortBy(fun v -> s.SmallestDistance.[v]) |> Seq.head
            let newVertices = s.Vertices.Remove u
            let neighbours = Set.intersect links.[u] newVertices
            let distU = s.SmallestDistance.[u]
            let update ((dist,prev) : Map<int*int,int>*Map<int*int,int*int>) (v:int*int)  = 
                let alternateDistance = distU + 1
                if alternateDistance < dist.[v] then
                    let di = dist.Remove v
                    let pr = prev.Remove v
                    di.Add(v, alternateDistance), pr.Add(v,u)
                else
                    dist,prev
            let newDistance , newPrevious = neighbours |> Set.fold update (s.SmallestDistance, s.PreviousVertex)
            let q = {Vertices = newVertices; SmallestDistance = newDistance; PreviousVertex = newPrevious}
            Some(q,q)
    Seq.unfold unfolder initialState |> Seq.last
let distanceMap = findSteps (0,0)

//runtime slightly more than 1 minute..

let part1 = 
    distanceMap.SmallestDistance |> Seq.map(fun t -> t.Value) |> Seq.max
let part2 = 
    distanceMap.SmallestDistance |> Seq.filter(fun t -> t.Value >= 1000 ) |> Seq.length