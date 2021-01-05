let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day15.txt")

type disc = 
    {
        Number : uint64
        Positions : uint64
        Position : uint64
    }

let parse str= 
    let pattern = @"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)."
    seq{for m in System.Text.RegularExpressions.Regex.Matches(str,pattern) do
        yield 
            {
                Number = m.Groups.[1].Value |> uint64
                Positions = m.Groups.[2].Value |> uint64
                Position = m.Groups.[3].Value |> uint64
            }}
    |> Seq.toList

let inputDiscs = parse input

let fallThruTime discs = 
    Seq.initInfinite uint64
    |> Seq.skipWhile(fun time -> 
        discs |> List.sumBy(fun disc -> (time + disc.Position + disc.Number)%disc.Positions ) <> 0UL)
    |> Seq.head

let part1 = 
    fallThruTime inputDiscs

let part2 = 
    let numberOfDiscs = inputDiscs |> List.length |> uint64
    let newDisc = {Number = numberOfDiscs + 1UL; Positions = 11UL; Position = 0UL}
    fallThruTime (newDisc::inputDiscs)