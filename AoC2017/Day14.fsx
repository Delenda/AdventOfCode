#load "Day10.fsx"
let input = "jzgqcdpd"
let computeBits str = 
    let calculateKnotHash = Day10.question2
    let convertToBinary (x:char) =
        match x with
        | 'a' | 'b' | 'c' | 'd'| 'e' | 'f' -> 10 + (x |> int) - ('a' |> int)
        | _ -> x |> string |> int
        |> fun i -> System.Convert.ToString(i,2).PadLeft(4,'0')
    str 
    |> calculateKnotHash 
    |> Seq.map convertToBinary
    |> Seq.reduce (+)

let computeGrid str = 
    [0..127] 
    |> List.map(fun i -> str + "-" + i.ToString()) 
    |> List.map computeBits
    |> List.reduce (+)

let grid = computeGrid input

let question1  =
    grid |> fun s -> s.Replace("0","").Length

let question2 = 
    let nodes = grid |> Seq.mapi(fun i x -> match x with | '1' -> Some (i/128,i%128) | _ -> None ) |> Seq.choose id |> Set.ofSeq
    let unfoldGroup unseen (group,frontier) =
        match frontier |> Seq.length with
        | 0 -> None
        | _ -> 
            let newFrontier = Set.difference (frontier |> Seq.collect(fun (x,y) -> [(x, y - 1); (x, y + 1); (x - 1, y);(x + 1,y)] ) |> Set.ofSeq |> Set.intersect unseen) group
            let expandedGroup = group |> Set.union newFrontier 
            Some(expandedGroup, (expandedGroup,newFrontier))

    let findNextGroup (unseen : (int*int) Set) =
        match unseen.Count with
        | 0 -> None
        | _ -> 
            let newElm = unseen |> Seq.take 1 |> Set.ofSeq
            let newGroup =  Seq.unfold (unfoldGroup unseen) (newElm,  newElm) |> Seq.last
            Some (newGroup, Set.difference unseen newGroup)

    Seq.unfold findNextGroup nodes |> Seq.length