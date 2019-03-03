let input = System.IO.File.ReadAllLines(@"C:\Users\Lars\source\repos\AoC2018\Input\Day13.txt")

type direction =
    | Up
    | Down
    | Left
    | Right

type intersection = 
    | LeftTurn
    | Straight
    | RightTurn

let nextIntersection= 
    function
    | LeftTurn -> Straight
    | Straight -> RightTurn
    | RightTurn -> LeftTurn

let directions (d, i, c) =
    match d, i, c with
    | Up, _, '\\' -> Left, i
    | Up, _, '/' -> Right, i
    | Down, _, '\\' -> Right,i
    | Down, _, '/' -> Left,i
    | Right, _, '\\' -> Down,i
    | Right, _, '/' -> Up,i
    | Left, _ , '\\' -> Up,i
    | Left, _, '/' -> Down,i
    | Up, RightTurn, '+' -> Right, nextIntersection i
    | Up, Straight, '+' -> Up, nextIntersection i
    | Up, LeftTurn, '+' -> Left, nextIntersection i
    | Down, RightTurn, '+' -> Left ,nextIntersection i
    | Down, Straight, '+' -> Down, nextIntersection i
    | Down, LeftTurn, '+' -> Right, nextIntersection i
    | Left, RightTurn, '+' -> Up, nextIntersection i
    | Left, Straight, '+' -> Left, nextIntersection i
    | Left, LeftTurn, '+' -> Down, nextIntersection i
    | Right, RightTurn, '+' -> Down, nextIntersection i
    | Right, Straight, '+' -> Right, nextIntersection i
    | Right, LeftTurn, '+' -> Up, nextIntersection i
    | _ -> d,i

type cart =
    {
        Direction : direction
        Intersection : intersection
        Position : int*int
    }

let carts = 
    input 
    |> Array.indexed
    |> Array.fold( fun s (t:int*string) -> 
                    (snd t).ToCharArray() 
                    |> Array.indexed 
                    |> Array.fold (fun (r : Set<cart>) u ->
                                        match (snd u) with
                                        | '<' -> 
                                            r.Add { Direction  = Left; Intersection = LeftTurn; Position = (fst u, fst t)}
                                        | 'v' -> 
                                            r.Add { Direction  = Down; Intersection = LeftTurn; Position = (fst u, fst t)}
                                        | '>' -> 
                                            r.Add { Direction  = Right; Intersection = LeftTurn; Position = (fst u, fst t)}
                                        | '^' -> 
                                            r.Add { Direction  = Up; Intersection = LeftTurn; Position = (fst u, fst t)}
                                        | _ -> r                                        
                    ) s
    ) Set.empty

let replace p = 
    match p with
    | '<' | '>' -> '-'
    | 'v' | '^' -> '|'
    | _ -> p

let kort = 
    input
    |> Array.map(fun s -> s.ToCharArray() |> Array.map replace )

let step (mp: char array array) crt = 
    let newPos = 
        match crt.Direction with
        | Right -> (fst crt.Position + 1, snd crt.Position)
        | Left -> (fst crt.Position - 1, snd crt.Position)
        | Up -> (fst crt.Position, snd crt.Position - 1)
        | Down -> (fst crt.Position, snd crt.Position + 1)
    let c = mp.[(snd newPos)].[(fst newPos)]
    let newDir, newInt = directions (crt.Direction , crt.Intersection, c) 
    {Direction = newDir; Intersection = newInt; Position = newPos}

let swap (x,y) = (y,x)

let tick mp (crts : Set<cart>) = 
    let cs = 
        crts 
        |> Seq.sortBy(fun c -> swap c.Position)
        |> Array.ofSeq
    let moved = cs |> Array.map(fun c -> step mp c)
    let rv =
        [1..cs.Length]
        |> List.map(fun i -> 
                        let a = moved |> Array.take i
                        let b = cs |> Array.skip i
                        let k = a |> Array.fold(fun (s:Set<cart>) t -> s.Add t) Set.empty
                        b |> Array.fold(fun (s:Set<cart>) t -> s.Add t) k)
    rv |> Seq.ofList

let tickWithRemoval mp (crts : Set<cart>) = 
    let cs = 
        crts 
        |> Seq.sortBy(fun c -> swap c.Position)
        |> List.ofSeq
    let unfolder ((x,y): (cart list) * (cart list))  =
        if y.IsEmpty then
            None
        else
            let c, newY = 
                let o = step mp y.Head
                let ny = 
                    [o]@(y.Tail)
                    |> List.groupBy(fun t -> t.Position)
                    |> List.filter(fun r -> (snd r) |> List.length = 1)
                    |> List.collect snd
                    |> List.sortBy(fun gg -> swap gg.Position)
                if ny |> List.contains o then
                    [o],(ny |> List.filter(fun gg -> gg <> o))
                else
                    [],ny
            let newX = 
                c@x
                |> List.groupBy(fun t -> t.Position)
                |> List.filter(fun r -> (snd r) |> List.length = 1)
                |> List.collect snd
            let T = newX@newY |> Set.ofList
            let q = (newX,newY)
            Some(T,q)

    Seq.unfold unfolder ([],cs)
 
let part1 = 
    Seq.unfold (fun s -> 
                    let q = tick kort s
                    Some(q,q |> Seq.last)
                    ) carts
    |> Seq.collect id
    |> Seq.filter(fun s -> s |> Seq.groupBy(fun c -> c.Position) |> Seq.length < Seq.length s)
    |> Seq.take 1
    |> Seq.exactlyOne
    |> Seq.groupBy(fun s -> s.Position)
    |> Seq.filter(fun s -> (snd s) |> Seq.length > 1)
    |> Seq.take 1
    |> Seq.exactlyOne
    |> fun s -> fst s

let part2 = 
    Seq.unfold (fun s -> 
                    let q = tickWithRemoval kort s 
                    Some(q,q |> Seq.last)
                    ) carts
    |> Seq.collect id 
    |> Seq.filter(fun t -> t.Count = 1)
    |> Seq.take 1
    |> Seq.exactlyOne
    |> Seq.exactlyOne
    |> fun s -> s.Position

