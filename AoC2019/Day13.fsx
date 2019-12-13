let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day13.txt") 

#load "IntCode.fsx"

let modifiedInput =
    input.Split(',')
    |> Array.mapi(fun i c-> if i = 0 then "2" else c)
    |> String.concat ","

let part1 = 
    IntCode.runProgram input []
    |> List.rev 
    |> List.chunkBySize 3 
    |> List.map(fun x -> x.Tail.Tail |> List.last) 
    |> List.filter(fun d -> d = 2L) 
    |> List.length 

let formatOutput op = 
    op
    |> List.rev
    |> List.chunkBySize 3
    |> List.fold(fun ((s,score):Map<int64*int64,string>*int64) d -> 
        let x = d.Head
        let y = d.Tail.Head
        let v = d.Tail.Tail.Head
        if x = -1L && y = 0L then 
            (s, v)
        else
            let c = 
                match v with
                | 0L -> " "
                | 1L -> "#"
                | 2L -> "&"
                | 3L -> "T"
                | 4L -> "Ø"
                | _ -> failwith (sprintf "Unexpected value %d" v)
            s.Add((x,y), c), score) (Map.empty, 0L)

let print (op:int64 list) =
    let map,score = formatOutput op
    let blocks = map |> Seq.filter(fun k -> k.Value = "&") |> Seq.length
    let minX = map |> Seq.map(fun k -> fst k.Key) |> Seq.min
    let maxX = map |> Seq.map(fun k -> fst k.Key) |> Seq.max
    let minY = map |> Seq.map(fun k -> snd k.Key) |> Seq.min
    let maxY = map |> Seq.map(fun k -> snd k.Key) |> Seq.max
    seq{for i in [minY..maxY] do yield seq{for j in [minX..maxX] do yield map.[(j,i)]} |> String.concat ""}
    |> Seq.iter(System.Console.WriteLine)  
    System.Console.WriteLine (sprintf "Score: %d. Blocks: %d" score blocks)

let play ((state, score): IntCode.programState * int64 option) =
    if score.IsSome then 
        None
    else
        let (pos, mem, op, _) as progstate= Seq.unfold IntCode.programStep state |> Seq.last
        let map,score = op |> formatOutput
        let blocks = map |> Seq.filter(fun k -> k.Value = "&") |> Seq.length
        if blocks = 0 then 
            Some(score, (state, Some score))
        else
            let ball = map |> Seq.filter(fun k -> k.Value = "Ø") |> Seq.map(fun k -> k.Key) |> Seq.exactlyOne
            let paddle = map |> Seq.filter(fun k -> k.Value = "T") |> Seq.map(fun k -> k.Key) |> Seq.exactlyOne
            let joystick = 
                if fst ball = fst paddle then 
                    0L 
                else if fst ball < fst paddle then 
                    -1L 
                else 
                    1L
            Some(score, ((pos,mem,op,[joystick]), None)) //replace score by op for animation

let part2 = 
    let mem = (IntCode.createMemory modifiedInput).Add(-1L, 0L)
    Seq.unfold play ((0L, mem, [], [1L]), None) |>Seq.last