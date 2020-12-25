let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day24.txt")

type direction = 
    | West
    | East
    | NorthWest
    | SouthWest
    | NorthEast
    | SouthEast

type flag = |North|South 

let parse (line:string) =
    line.ToCharArray()
    |> Seq.fold(fun (flag, dirs) c -> 
        let dir, flag = 
            if c = 'n' then None, Some North else
            if c = 's' then None, Some South else
            if c = 'e' && flag = Some North then Some NorthEast, None else
            if c = 'e' && flag = Some South then Some SouthEast, None else
            if c = 'e' && flag = None then Some East, None else
            if c = 'w' && flag = Some North then Some NorthWest, None else
            if c = 'w' && flag = Some South then Some SouthWest, None else
            if c = 'w' && flag = None then Some West, None else
            failwith (sprintf "Failed to parse: %s" (string c))
        flag, dir::dirs
            ) (None, [])
    |> snd
    |> List.choose id
    |> List.rev

let instructions = 
    input 
    |> Array.map parse 

let walk (path : direction list) =
    path
    |> List.fold(fun (x,y) dir -> 
        match dir with
        | East -> (x + 2, y)
        | West -> (x - 2, y)
        | NorthWest -> (x - 1, y + 1)
        | SouthWest -> (x - 1, y - 1)
        | NorthEast -> (x + 1, y + 1)
        | SouthEast -> (x + 1, y - 1)
        ) (0,0)
    
let initialFloor = 
    instructions
    |> Seq.fold(fun (floor: Set<int*int>) path -> 
        let tile = walk path
        if floor.Contains tile then (floor.Remove tile) else (floor.Add tile)
        ) Set.empty

let neighbor (x,y) : Set<int*int>= 
    [(x+2,y); (x-2,y); (x+1,y+1); (x+1,y-1); (x-1,y+1); (x-1,y-1)] |> Set

let passDay (floor : Set<int*int>) =
    let candidates = 
        floor
        |> Seq.map neighbor
        |> Set.unionMany
    candidates
    |> Set.filter(fun tile -> 
        let nbs = Set.intersect (neighbor tile) floor
        if floor.Contains tile then
            if nbs.Count = 0 || nbs.Count > 2 then false else true
        else
            if nbs.Count = 2 then true else false)

let part1 = initialFloor.Count

let part2 = 
    Seq.unfold(fun s -> 
        let newFloor = passDay s
        Some(newFloor, newFloor)) initialFloor
    |> Seq.take 100
    |> Seq.last
    |> Set.count