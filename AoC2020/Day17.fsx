let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day17.txt")

open System.Collections.Generic

let parse cubeConstructor lines=
    let grid = new HashSet<int64 list>()
    lines
    |> Seq.indexed
    |> Seq.iter(fun (row, line) ->
        line
        |> Seq.indexed
        |> Seq.filter(fun (i,c) -> c = '#')
        |> Seq.iter(fun (column, c) -> grid.Add(cubeConstructor row column) |> ignore))
    grid

let adjacent cube =
    let rec adj (cube : int64 list) : int64 list seq=
        match cube with
        | [] -> [[]] |> List.toSeq
        | x::xs -> adj xs |> Seq.collect (fun y -> [(x-1L)::y; x::y; (x+1L)::y])
    adj cube |> Seq.filter(fun l -> l <> cube)

let step (grid:HashSet<int64 list>) =
    let remove   = new HashSet<int64 list>()
    let inactive = new HashSet<int64 list>()
    let add      = new HashSet<int64 list>()
    grid |> Seq.iter(fun cube ->
        let count =
            adjacent cube
            |> Seq.fold(fun cnt nb ->
                if grid.Contains nb then
                    cnt + 1
                else
                    inactive.Add nb |> ignore
                    cnt) 0
        if count <> 2 && count <> 3 then remove.Add cube |>ignore)
    inactive |> Seq.iter(fun cube ->
        let count = adjacent cube |> Seq.filter(fun nb -> grid.Contains nb) |> Seq.length
        if count = 3 then add.Add cube |> ignore
        )
    add    |> Seq.iter(fun cube -> grid.Add    cube |> ignore)
    remove |> Seq.iter(fun cube -> grid.Remove cube |> ignore)

let conwayGameOfLife cubeConstructor =
    let grid = parse cubeConstructor input
    [1..6] |> List.iter(fun _ -> step grid )
    grid.Count

let part1 = conwayGameOfLife (fun row col -> [int64 row; int64 col; 0L])
let part2 = conwayGameOfLife (fun row col -> [int64 row; int64 col; 0L; 0L])