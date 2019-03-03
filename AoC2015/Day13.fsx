let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day13.txt")

type info = { source : string; target: string; weight : int}

let getInfo (str:string) =
    let s = str.Split(';')
    {source = s.[0]; target = s.[2]; weight = s.[1] |> int}

let infos = input |> Array.map(fun x -> x.Replace(" would lose ", ";-").Replace(" would gain ",";").Replace(" happiness units by sitting next to ",";").Replace(".","")) |> Array.map getInfo |> List.ofArray



let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))



let getWeight s src tgt =
    s |> List.filter(fun i -> i.source = src && i.target = tgt) |> List.sumBy(fun x -> x.weight)

let getScore s (seating: string list) =
    [0..(seating.Length - 1)] |> List.sumBy(fun i -> getWeight s (seating.Item i) (seating.Item ((i + 1)%(seating.Length))))

let getScores s (seating:string list)=
    getScore s seating + getScore s (seating |> List.rev)

let calculateScore infs =
    let persons = infs |> List.map(fun s -> s.source) |> List.distinct
    let perms = permutations persons |> Seq.filter(fun x -> x.Head = "Bob")
    perms |> Seq.maxBy (getScores infs) |> (getScores infs)

let question1 = 
    calculateScore infos

let question2 = 
    let expandedInfos = infos @ (infos |> List.map(fun x -> x.target) |> List.distinct |> List.map(fun x -> [{target = "Lars"; source = x; weight = 0}; {target = x; source = "Lars"; weight = 0}]) |> List.collect id    )
    calculateScore expandedInfos