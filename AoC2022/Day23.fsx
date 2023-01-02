let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day23.txt") 

let print (elfs:Set<int*int>) = 
    let minx = elfs |> Seq.map fst |> Seq.min
    let maxx = elfs |> Seq.map fst |> Seq.max
    let miny = elfs |> Seq.map snd |> Seq.min
    let maxy = elfs |> Seq.map snd |> Seq.max
    [minx .. maxx] |> List.map(fun i -> [miny .. maxy] |> List.map(fun j -> if elfs.Contains(i,j) then "#" else ".") |> String.concat "")
    |> List.iter System.Console.WriteLine
    System.Console.WriteLine ""

let initialPositions = 
    input
    |> Array.mapi(fun i s -> s.ToCharArray() |> Array.indexed |> Array.filter(fun (_,c) -> c = '#') |> Array.map(fun (j,_) -> i,j) |> Set )
    |> Set.unionMany

let neighbors (x,y) = 
    seq{for dx in [-1;0;1] do
        for dy in [-1;0;1] do
        if dx<>0 || dy <> 0 then 
            yield (x+dx,y+dy)}
    |> Set

let directionOffsets = 
    [|
        [|(-1,-1); (-1,0);(-1,1)|] //north
        [|(1,-1); (1,0);(1,1)|] //south
        [|(-1,-1);(0,-1);(1,-1)|] //west
        [|(-1,1);(0,1);(1,1)|] //east
    |]

let proposition elfs direction (x,y):  (int*int) option=
    seq{for dir in [0..3] do
            let front = directionOffsets.[(dir + direction)%4] |> Array.map(fun (dx,dy) -> (x+dx,y+dy)) |> Set
            if Set.intersect front elfs |> Set.isEmpty then 
                let (dx,dy) = directionOffsets.[(dir + direction)%4].[1]
                yield (x +  dx, y + dy)
        }
    |> Seq.tryHead
    
let round (elfs,direction) = 
    //print elfs
    let candidates = elfs |> Set.filter(fun e -> Set.intersect (neighbors e) elfs |> Set.isEmpty |> not)
    if candidates.IsEmpty then None else
    let propositions = candidates |>  Seq.map (fun e ->  e, proposition elfs direction e) |> Seq.filter(fun (_,p) -> p.IsSome) |> Map
    let propCount = propositions |> Map.toSeq |> Seq.map snd |> Seq.countBy id |> Map
    let moves = propositions |> Map.filter(fun elf prop -> propCount.[prop] = 1)
    let newDir = (direction+1)%4
    let newElfs = moves |> Map.toSeq |> Seq.fold(fun (e:Set<int*int>) (elf,move) -> e.Remove(elf).Add(move.Value) ) elfs
    let q = newElfs,newDir
    Some(newElfs,q)    

let count (elfs:Set<int*int>) = 
    let minx = elfs |> Seq.map fst |> Seq.min
    let maxx = elfs |> Seq.map fst |> Seq.max
    let miny = elfs |> Seq.map snd |> Seq.min
    let maxy = elfs |> Seq.map snd |> Seq.max
    (maxx-minx+1)*(maxy-miny+1)-elfs.Count

let rounds = Seq.unfold round (initialPositions, 0)        

let part1 = rounds |> Seq.take 10 |> Seq.last |> count
let part2 = rounds |> Seq.length |> (+) 1 
