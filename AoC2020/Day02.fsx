let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day02.txt")

type passwordRule =  {lower : int; upper: int; digit : char; pw : string}

let passwords =
        let pattern = @"(\d+)-(\d+) (\w): (\w+)"
        let regex = System.Text.RegularExpressions.Regex pattern
        seq { for m in regex.Matches input do
                let lower = m.Groups.[1].Value |> int
                let upper = m.Groups.[2].Value |> int
                let digit = m.Groups.[3].Value |> char
                let pw    = m.Groups.[4].Value
                yield {lower = lower; upper = upper; digit = digit; pw = pw}}
        |> Seq.toList

let part1 =
    passwords
    |> List.filter(fun pw ->
        let digitCount = pw.pw |> Seq.countBy id |> Map
        digitCount.TryFind (pw.digit)
        |> Option.map(fun count -> count <= pw.upper && count >= pw.lower)
        |> Option.defaultValue false
        )
    |> List.length

let part2 =
    passwords
    |> List.filter(fun pw ->
        let lowerDigit = pw.pw.[pw.lower - 1]
        let upperDigit = pw.pw.[pw.upper - 1]
        (lowerDigit = pw.digit && upperDigit <> pw.digit) || (lowerDigit <> pw.digit && upperDigit = pw.digit))
    |> List.length