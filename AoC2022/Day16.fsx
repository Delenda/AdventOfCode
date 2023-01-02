let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day16.txt") 

type valve = {Valve:string; Rate:int; Links: Set<string>}

let parse (line:string) = 
    let fields = line.Split(';')
    let regex = System.Text.RegularExpressions.Regex("Valve (\w+) has flow rate=(\d+)")
    let m = regex.Match line
    let valve = m.Groups.[1].Value 
    let rate = m.Groups.[2].Value |> int
    let links = fields.[1].Replace(",","").Split(' ') |> Seq.rev |> Seq.takeWhile(fun s -> s.Length = 2) |> Set
    valve,{Valve = valve; Rate = rate; Links = links}
    
let valves = input |> Array.map parse |> Map

let rate0 = valves |> Map.filter(fun k v -> v.Rate = 0) |> Map.toSeq |> Seq.map fst |> Set

let shortestDist valve =
    let nb valve = valves.[valve].Links
    Seq.unfold(fun ((frontier, depth, seen): Set<string>*int*Map<string,int>) -> 
        if frontier.IsEmpty then None else
        let newDepth = depth + 1
        let newFrontier = frontier |> Seq.map nb |> Set.unionMany |> Seq.filter(seen.ContainsKey >> not) |> Set
        let newSeen = newFrontier |> Seq.fold(fun (s:Map<string,int>) t -> s.Add(t,newDepth)) seen
        let q = newFrontier,newDepth,newSeen
        Some(q,q)) (Set [valve], 0, Map[valve,0])
    |> Seq.last
    |> fun (a,b,c) -> c |> Map.filter(fun k _  -> rate0.Contains k |> not)

let dists = valves |> Map.filter(fun k _ -> rate0.Contains k |> not || k = "AA") |> Map.map(fun k v -> shortestDist k)

let rec release (elephant:bool) (opened:Set<string>) (extra:Set<string>) time valve = 
    let akkRate = opened |> Seq.sumBy(fun v -> valves.[v].Rate)
    let r  =
        dists.[valve] 
        |> Map.filter(fun k dist-> k |> opened.Contains |> not && dist < time - 1 && (extra.Contains k |> not)) 
    if r.IsEmpty then
        if elephant then
            akkRate*time + (release false Set.empty opened 26 "AA") 
        else
            akkRate*time
    else
        Seq.append
            (if elephant then [akkRate*time + release false Set.empty opened 26 "AA" ] else [])//Non-greedy: let the elephant deal with the remaining valves
            (r |> Map.toSeq |> Seq.map(fun (k,v) -> akkRate*(v+1) + release elephant (opened.Add k) extra (time - v - 1) k) ) //Greedy: open one more valve if possible
        |> Seq.max

let sw = System.Diagnostics.Stopwatch.StartNew()
let part1 = release false Set.empty Set.empty 30 "AA"
let part2 = release true  Set.empty Set.empty 26 "AA"
let duration = sw.Elapsed.TotalSeconds