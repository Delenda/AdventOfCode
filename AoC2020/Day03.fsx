let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day03.txt")

let treeCount (xSlope,ySlope) =
    input
    |> Array.chunkBySize ySlope
    |> Array.map Array.head
    |> Array.fold(fun s t ->
        let (x,cnt) = s
        let treeCount =
            if t.[x%t.Length] = '#' then 1L else 0L

        (x + xSlope, cnt + treeCount)
        ) (0, 0L)
    |> snd

let part1 =
    treeCount (3,1)

let part2 =
    [(1,1);(3,1);(5,1);(7,1);(1,2)]
    |> List.map treeCount
    |> List.reduce(*)