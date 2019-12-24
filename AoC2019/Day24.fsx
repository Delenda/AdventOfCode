let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +   @"\Input\Day24.txt") 

let field a= 
    Array.concat [|[|"....."|]; a; [|"....."|]|]
    |> Array.map(fun s -> s.PadLeft(6,'.').PadRight(7,'.'))
    |> Array.map(fun s -> s.ToCharArray())

let evolve (a:char array array) = 
    a 
    |> Array.mapi(fun i row -> 
        if i = 0 || i = 6 then row else
        row |> Array.mapi(fun j c -> 
            if j = 0 || j = 6 then '.' else
            let count = 
                seq{for (x,y) in [(-1,0); (1,0); (0,-1);(0,1)] do
                        if a.[i+x].[j+y] = '#' then yield 1 else yield 0 }
                |> Seq.sum
            if c = '#' then
                if count = 1 then '#' else '.'
            else
                if count = 1 || count = 2 then '#' else '.'
            ))

let repeat a = 
    let step ((f, seen): char array array * Set<char array array>) =
        if seen.Contains f then None else
        let newF = evolve f
        let newSeen = seen.Add f
        Some(newF, (newF, newSeen))
    Seq.unfold step (a, Set.empty) |> Seq.last

let biodiversity (a:char array array) =
    a
    |> Array.tail
    |> Array.take 5
    |> Array.map(Array.tail >> Array.take 5)
    |> Array.collect id
    |> Array.indexed
    |> Array.filter(fun x -> snd x = '#')
    |> Array.sumBy(fun (i,x) -> if i = 0 then 1 else List.replicate i 2 |> List.reduce(*))

let testfield = 
    [|
        "....#"
        "#..#."
        "#..##"
        "..#.."
        "#...."
    |] 

let test1 = biodiversity (testfield |> field |> repeat) = 2129920

let part1 = 
    input
    |> field
    |> repeat
    |> biodiversity

type cell = 
    {level : int; x : int; y : int}

let adjacent (c:cell) =
    if (c.x = 1 && c.y = 1) || (c.x = 3 && c.y = 3) || (c.x = 1 && c.y = 3) || (c.x = 3 && c.y = 1) then
        [
            {c with x = c.x - 1}
            {c with x = c.x + 1}
            {c with y = c.y - 1}
            {c with y = c.y + 1}
        ] |> Set
    else if c.x = 0 && c.y = 0 then
        [
          {c with y = 1}
          {c with x = 1}
          {x = 2; y = 1; level = c.level - 1}
          {x = 1; y = 2; level = c.level - 1}
        ] |> Set
    else if c.x = 4 && c.y = 0 then
        [
          {c with y = 1}
          {c with x = 3}
          {x = 2; y = 1; level = c.level - 1}
          {x = 3; y = 2; level = c.level - 1}
        ] |> Set
    else if c.x = 4 && c.y = 4 then
        [
          {c with y = 3}
          {c with x = 3}
          {x = 2; y = 3; level = c.level - 1}
          {x = 3; y = 2; level = c.level - 1}
        ] |> Set
    else if c.x = 0 && c.y = 4 then
        [
          {c with y = 3}
          {c with x = 1}
          {x = 2; y = 3; level = c.level - 1}
          {x = 1; y = 2; level = c.level - 1}
        ] |> Set
    else if c.y = 0 then
        [
          {c with y = 1}
          {c with x = c.x + 1}
          {c with x = c.x - 1}
          {x = 2; y = 1; level = c.level - 1}
        ] |> Set
    else if c.y = 4 then
        [
          {c with y = 3}
          {c with x = c.x + 1}
          {c with x = c.x - 1}
          {x = 2; y = 3; level = c.level - 1}
        ] |> Set
    else if c.x = 0 then
        [
          {c with x = 1}
          {c with y = c.y + 1}
          {c with y = c.y - 1}
          {x = 1; y = 2; level = c.level - 1}
        ] |> Set
    else if c.x = 4 then
        [
          {c with x = 3}
          {c with y = c.y + 1}
          {c with y = c.y - 1}
          {x = 3; y = 2; level = c.level - 1}
        ] |> Set
    else if c.x = 2 && c.y = 1 then
        [
          {c with y = 0}
          {c with x = c.x + 1}
          {c with x = c.x - 1}
          {x = 0; y = 0; level = c.level + 1}
          {x = 1; y = 0; level = c.level + 1}
          {x = 2; y = 0; level = c.level + 1}
          {x = 3; y = 0; level = c.level + 1}
          {x = 4; y = 0; level = c.level + 1}
        ] |> Set
    else if c.x = 2 && c.y = 3 then
        [
          {c with y = 4}
          {c with x = c.x + 1}
          {c with x = c.x - 1}
          {x = 0; y = 4; level = c.level + 1}
          {x = 1; y = 4; level = c.level + 1}
          {x = 2; y = 4; level = c.level + 1}
          {x = 3; y = 4; level = c.level + 1}
          {x = 4; y = 4; level = c.level + 1}
        ] |> Set
    else if c.x = 1 && c.y = 2 then
        [
          {c with x = 0}
          {c with y = c.y + 1}
          {c with y = c.y - 1}
          {x = 0; y = 0; level = c.level + 1}
          {x = 0; y = 1; level = c.level + 1}
          {x = 0; y = 2; level = c.level + 1}
          {x = 0; y = 3; level = c.level + 1}
          {x = 0; y = 4; level = c.level + 1}
        ] |> Set
    else if c.x = 3 && c.y = 2 then
        [
          {c with x = 4}
          {c with y = c.y + 1}
          {c with y = c.y - 1}
          {x = 4; y = 0; level = c.level + 1}
          {x = 4; y = 1; level = c.level + 1}
          {x = 4; y = 2; level = c.level + 1}
          {x = 4; y = 3; level = c.level + 1}
          {x = 4; y = 4; level = c.level + 1}
        ] |> Set
    else
        failwith (sprintf "Unexpected coordinates: %d %d" c.x c.y)
     
let grow (f:Set<cell>) =
    let candidateCells = 
        f 
        |> Set.map adjacent 
        |> Set.unionMany
        |> Set.union f
    candidateCells
    |> Set.filter(fun c -> 
        let n = adjacent c
        let adjacents = Set.intersect n f
        if f.Contains c then adjacents.Count = 1 else (adjacents.Count = 1 || adjacents.Count = 2))

let setField (a:string array) =
    a
    |> Array.mapi(fun i s-> 
        s.ToCharArray() |> Array.mapi(fun j c -> if c = '#' then {x = j; y = i; level = 0} |> Some else None) |> Array.choose id)
    |> Array.collect id
    |> Set

let print f =
    f 
    |> Seq.groupBy(fun c -> c.level)
    |> Seq.iter(fun (l,d) -> 
        System.Console.WriteLine(sprintf "Depth %d:" l)
        let k = d |> Seq.map(fun c -> (c.x, c.y)) |> Set
        [0..4] 
        |> Seq.iter(fun y -> 
            [0..4] |> Seq.map(fun x -> if (x,y) = (2,2) then "?" else if k.Contains (x,y) then "#" else ".") 
            |> String.concat "" 
            |> System.Console.WriteLine
            )
        )

let test2 = testfield |> setField |> (List.replicate 10 grow |> List.reduce(>>)) |> Set.count = 99

let part2 = 
    input
    |> setField
    |> (List.replicate 200 grow |> List.reduce(>>))
    |> Set.count