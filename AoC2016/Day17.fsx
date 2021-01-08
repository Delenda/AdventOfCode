let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day17.txt")

let md5 = System.Security.Cryptography.MD5.Create()

let adjacent (passcode:string) (node:string) = 
    md5.ComputeHash((passcode+node).ToCharArray() |> Array.map byte)
    |> Seq.take 2
    |> Seq.collect(fun b -> b.ToString("x2").ToCharArray()) 
    |> Seq.indexed
    |> Seq.filter(fun (_, b) -> int b > int 'a')
    |> Seq.map(fun (idx, _) -> 
        match idx with
        | 0 -> "U"
        | 1 -> "D"
        | 2 -> "L"
        | 3 -> "R"
        | _ -> failwith "unexpected index")

let newPosition ((x,y) : int*int) (direction:string) =
    match direction with
    | "U" -> (x,y-1)
    | "D" -> (x,y+1)
    | "L" -> (x-1,y)
    | "R" -> (x+1,y)
    | _ -> failwith "unexpected direction"

let breadthFirstSearch passcode = 
    Seq.unfold(fun (frontier:Set<(int*int)*string>) -> 
        if frontier.IsEmpty then None else
        if fst (frontier.MaximumElement) = (3,3) then Some(frontier.MaximumElement |> snd |> Some , frontier.Remove (frontier.MaximumElement)) else
        let newFrontier = 
            frontier 
            |> Seq.map (fun (pos, path) -> pos, path, adjacent passcode path )
            |> Seq.collect(fun (pos, path, directions) -> directions |> Seq.map(fun y -> (newPosition pos y),path + y))
            |> Seq.filter(fun ((x,y), _) -> x >=0 && x<=3 && y>= 0 && y<= 3)
            |> Set
        Some(None, newFrontier)
        ) (Set [(0,0),""])
    |> Seq.choose id
            
let part1 = breadthFirstSearch input |> Seq.head
let part2 = breadthFirstSearch input |> Seq.last |> String.length