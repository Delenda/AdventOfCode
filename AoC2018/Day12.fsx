open System.Numerics

let input = System.IO.File.ReadAllLines(@"C:\Users\Lars\source\repos\AoC2018\Input\Day12.txt")

let pots = 
    input 
    |> Array.head 
    |> fun s -> s.Replace("initial state: ","").ToCharArray()
    |> Array.indexed
    |> Array.filter(fun s -> snd s='#')
    |> Array.map fst
    |> Array.fold (fun (s : Set<int>) t -> s.Add t) Set.empty

let rules = 
    let folder (s : Set<bool*bool*bool*bool*bool>) (t:string array ) = 
        let rule =
            let vals = 
                t.[0].ToCharArray()
                |> Array.map(fun c -> if c = '#' then true else false)
            (vals.[0], vals.[1], vals.[2], vals.[3], vals.[4])
        s.Add rule
    input 
    |> Array.tail
    |> Array.tail
    |> Array.map(fun t -> t.Replace(" => ",";").Split(';') )
    |> Array.filter(fun t -> t.[1] = "#")
    |> Array.fold folder Set.empty

let grow (rulz : Set<bool*bool*bool*bool*bool>) (state : Set<int>) = 
    let lowIdx = state.MinimumElement - 5
    let highIdx = state.MaximumElement + 5
    Seq.init (highIdx - lowIdx + 1) (fun t -> lowIdx + t)
    |> Seq.filter(fun t -> rulz.Contains(state.Contains(t-2), state.Contains(t-1), state.Contains(t), state.Contains(t+1), state.Contains(t+2) ) ) 
    |> Set.ofSeq

let size n = 
    [1UL..n]
    |> List.fold(fun s t -> grow rules s) pots
    |> Seq.sum
    |> uint64

let zerobased (pts: Set<int>) = 
    let minIdx = pts.MinimumElement
    pts |> Set.map(fun t -> t-minIdx)
        
let isFix rulz pts = 
    let z = zerobased pts
    z = zerobased (grow rulz z)

let firstRepeat rulz pts = 
    Seq.unfold (fun s -> 
                    let q = grow rulz s
                    Some(q,q)) pts
    |> Seq.indexed
    |> Seq.filter(fun x -> (snd >> isFix rulz) x )
    |> Seq.take 1
    |> Seq.exactlyOne
    |> fst
    |> (+) 1
    |> uint64

let part1 = 
    size 20UL

let part2 = 
    let fix = firstRepeat rules pots
    let valFix = size fix
    let delta = size (fix+1UL) - valFix 
    valFix + delta*(50000000000UL- fix)