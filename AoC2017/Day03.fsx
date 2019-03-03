let input = 277678

let values = new System.Collections.Generic.Dictionary<int*int, int>()

values.Add((0,0),1)

type state = {pos: int*int; direction : int*int; values : Map<int*int,int>}


let turn direction =
    (- snd direction, fst direction )

let move position direction =
    let newDirection = turn direction
    let turnMove = (fst position + fst newDirection, snd position + snd newDirection)
    match values.ContainsKey turnMove with
    | false -> turnMove, newDirection
    | true -> (fst position + fst direction, snd position + snd direction), direction

let findNeighbours position  =
    let x = fst position
    let y = snd position
    [(x-1,y-1); (x-1,y); (x-1,y+1); (x,y-1); (x,y+1); (x+1,y-1); (x+1,y); (x+1,y+1)]

let walk (position, direction) =
    let newP, newD = move position direction
    let nbs = findNeighbours newP
    let fieldValue = nbs |> List.filter(values.ContainsKey) |> List.map(fun p -> values.[p]) |> List.sum
    values.Add(newP, fieldValue)
    (newP, newD)

[1..100] |> List.fold(fun s t -> walk s) ((0,0), (0,-1))

values.Values |> Seq.filter(fun i -> i > input) |> Seq.head