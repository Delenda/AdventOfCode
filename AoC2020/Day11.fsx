let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day11.txt")

let seating cellTransform (initialChart:string array)=
    Seq.unfold(fun (chart: string array) ->
        let newChart =
            chart
            |> Array.mapi(fun i line ->
                line.ToCharArray() |> Array.mapi(fun j c -> cellTransform i j c chart) |> String.concat "")
        if newChart = chart then None else
        Some (newChart, newChart)
        ) initialChart
    |> Seq.last
    |> Array.map(fun s -> s.ToCharArray() |> Seq.filter(fun c -> c = '#') |> Seq.length)
    |> Array.sum

let part1 =
    let cellTransform line column celltype (m :string array)=
        if celltype = '.' then "." else
        let occupied =
            seq {
                for r in [max 0 (column-1)..min (input.[0].Length-1) (column+1)] do
                for s in [max 0 (line-1)..min (input.Length-1) (line+1)] do
                    if (r,s) <> (column, line) then yield m.[s].[r]}
            |> Seq.filter(fun k -> k = '#')
            |> Seq.length
        if celltype = 'L' then
            if occupied = 0 then "#" else "L"
        else
            if occupied >= 4 then "L" else "#"
    seating cellTransform input

let part2 =
    let cellTransform line column celltype (m :string array)=
        if celltype = '.' then "." else
        let occupied =
            let directions =
                seq {
                    for r in [max 0 (column-1)..min (m.[0].Length-1) (column+1)] do
                    for s in [max 0 (line-1)..min (m.Length-1) (line+1)] do
                        if (r,s) <> (column, line) then yield (r-column, s-line)}
                |> Seq.toList
            seq{for (x,y) in directions do
                let firstVisible =
                    Seq.initInfinite(fun i -> (i+1)*x + column, (i+1)*y + line)
                    |> Seq.takeWhile(fun (a,b) -> a>=0 && b>=0 && a <= (m.[0].Length-1) && b <= (m.Length-1))
                    |> Seq.map(fun (a,b) -> m.[b].[a])
                    |> Seq.toList
                    |> Seq.filter(fun c -> c = 'L' || c = '#')
                    |> Seq.tryHead
                if firstVisible.IsSome then yield firstVisible.Value}
            |> Seq.filter(fun c -> c = '#')
            |> Seq.length
        if celltype = 'L' then
            if occupied = 0 then "#" else "L"
        else
            if occupied >= 5 then "L" else "#"
    seating cellTransform input