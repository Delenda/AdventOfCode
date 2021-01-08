let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day20.txt") 

type interval = { Begin : int64; End: int64}

let parse (line:string) = 
    let fields = line.Split('-')
    {Begin = fields.[0] |> int64; End = fields.[1] |> int64}

let part1 = 
    input
    |> Seq.map parse
    |> Seq.sortBy(fun x -> x.Begin)
    |> Seq.fold(fun ((s,candidate): int64 option * int64) interval -> 
            if s.IsSome then (s,0L) else
            let found = 
                if interval.Begin > candidate then Some candidate else None
            let newCandidate = 
                if interval.End >= candidate then interval.End + 1L else candidate
            found, newCandidate
        ) (None, 0L)

let part2 = 
    input
    |> Seq.map parse
    |> Seq.sortBy(fun x -> x.Begin)
    |> Seq.fold(fun ((count,upperbound): int64 * int64) interval -> 
        let gap = interval.Begin - upperbound - 1L |> max 0L
        let newUpperbound = max upperbound interval.End
        count + gap, newUpperbound
        ) (0L, 0L)
    |> fst