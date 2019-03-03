let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day07.txt")

let programs = input |> Array.map(fun s -> s.Split(' ').[0])
let findSubprograms (s : string) =
    match s.Contains("->") with
    | true ->
        let idx = s.IndexOf("->")
        let r = s.Substring(idx + 3)
        r.Split(',') |> Array.map(fun s -> s.Replace(" ",""))
    | false -> [||]
let subprograms = input |> Array.filter(fun s -> s.Contains("->")) |> Array.collect findSubprograms

programs |> Array.filter(fun s -> not(subprograms |> Array.contains s )) // Question 1

//find weights
let weights = new System.Collections.Generic.Dictionary<string, int>()
let findProgram (s:string) =
    let name = s.Split(' ').[0]
    let idxl = s.IndexOf("(")
    let idxr = s.IndexOf(")")
    let weight = s.Substring(idxl+1, idxr - idxl - 1) |> int
    name, weight
input |> Array.map findProgram |> Array.iter(fun (x,y) -> weights.Add(x,y))

// find links
let getLinks (s:string) =
    let name = s.Split(' ').[0]
    findSubprograms s |> Array.map(fun y -> (name,y))
let links = input |> Array.collect getLinks

let rec findWeight pgm =
    let subPrograms = links |> Array.filter(fun x -> fst x = pgm)
    let subWeights = subPrograms |> Array.map (snd >> findWeight)
    weights.[pgm]  + (subWeights |> Array.sum)

let rec findErrorProgram pgm =
    let subPrograms = links |> Array.filter(fun x -> fst x = pgm) |> Array.map snd |> Array.map(fun x -> (x, findWeight x))
    let deviatingWeight = subPrograms |> Array.groupBy snd |> Array.filter(fun (x,y) -> y.Length = 1)
    match deviatingWeight.Length with
    | 0 -> pgm
    | _ -> deviatingWeight |> Array.exactlyOne |> snd |> Array.exactlyOne |> fst |> findErrorProgram

let findCorrectWeight rootPgm =
    let errorPgm = findErrorProgram rootPgm
    let parent = links |>  Array.filter(fun x -> snd x = errorPgm) |> Array.exactlyOne |> fst
    let correctWeight = links |> Array.filter(fun x -> fst x = parent) |> Array.map snd |> Array.map(fun x -> (x, findWeight x)) |> Array.groupBy snd |> Array.filter(fun (x,y) -> y.Length <> 1) |> Array.map fst |> Array.exactlyOne
    let pgmWeight = findWeight errorPgm
    weights.[errorPgm] + correctWeight - pgmWeight

findCorrectWeight "ykpsek" //Question 2