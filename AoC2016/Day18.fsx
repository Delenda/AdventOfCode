let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day18.txt")

let nextRow (row : char array) : char array=
    row 
    |> Array.mapi(fun i _ -> 
        let left = if i = 0 then '.' else row.[i-1]
        let right = if i = row.Length - 1 then '.' else row.[i+1]
        if left <> right then '^' else '.')

let expand rowCount (firstRow: string) =
    Seq.initInfinite id
    |> Seq.scan(fun row _ -> nextRow row) (firstRow.ToCharArray())
    |> Seq.take rowCount

let countSafeTiles rowCount (firstRow:string)=
     expand rowCount firstRow
     |> Seq.collect id 
     |> Seq.filter(fun c -> c = '.') 
     |> Seq.length

let part1 = countSafeTiles 40 input
let part2 = countSafeTiles 400000 input