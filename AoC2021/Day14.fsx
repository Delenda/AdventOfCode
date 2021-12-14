let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day14.txt") 

let template = input.[0].ToCharArray()

let insertions = 
    input
    |> Array.skip 2
    |> Array.map(fun (s : string)-> 
        let f = s.Split([|" -> "|], System.StringSplitOptions.RemoveEmptyEntries)
        (f.[0].[0],f.[0].[1]),f.[1].[0])
    |> Map

let AddCount (a: Map<char,int64>) (b:Map<char,int64>) = 
    (a |> Map.toSeq |> Seq.map fst |> Set) + (b |> Map.toSeq |> Seq.map fst |> Set)
    |> Seq.map(fun k -> k, (a.TryFind k |> Option.defaultValue 0L) + (b.TryFind k |> Option.defaultValue 0L))
    |> Map

let countPair (count:Map<char*char, Map<char,int64>>) pair= 
    if insertions.ContainsKey pair |> not then [fst pair; snd pair] |> Seq.countBy id |> Map |> Map.map(fun _ v -> int64 v) else
    let insertion = insertions.[pair]
    let pair1 = fst pair, insertion 
    let pair2 = insertion, snd pair
    let count1 = count.[pair1]
    let count2 = count.[pair2]
    let join = AddCount count1 count2
    let retval = join |> Map.map(fun k (v:int64) -> if k = insertion then (v - 1L) else v)
    retval

let initialCount = 
    let transPolymers = insertions |> Map.toSeq |> Seq.collect(fun ((a,b),c) -> [|a;b;c|]) |> Set
    let templatePols = template |> Set
    let allPolymers = transPolymers + templatePols 
    seq{for a in allPolymers do
        for b in allPolymers do
        yield (a,b)}
    |> Seq.map(fun a -> a, [fst a; snd a] |> Seq.countBy id |> Map |> Map.map(fun _ v -> int64 v))
    |> Map

let count n = 
    Seq.unfold(fun oldCount -> 
        let newCount = 
            oldCount
            |> Map.map(fun pair _ -> countPair oldCount pair)
        Some(newCount, newCount)) initialCount
    |> Seq.take n
    |> Seq.last

let countPolymer n = 
    let cnt = count n
    let pairCount = 
        template
        |> Array.windowed 2
        |> Array.map(fun x -> (x.[0],x.[1]))
        |> Array.map(fun pair -> cnt.[pair])
        |> Array.fold AddCount Map.empty
    let overlap = 
        template
        |> Array.tail
        |> Array.rev
        |> Array.tail
        |> Array.countBy id
        |> Map
        |> Map.map(fun _ v -> int64 -v)
    let count = AddCount pairCount overlap
    let maximum = count |> Map.toSeq |> Seq.maxBy snd
    let minimum = count |> Map.toSeq |> Seq.minBy snd
    snd maximum - snd minimum

let part1 = countPolymer 10
let part2 = countPolymer 40