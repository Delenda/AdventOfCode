let input = System.IO.File.ReadAllLines(@"D:\Src\Aoc2015\Input\Day19.txt")
let molecule = input |> Array.last

type substitution  =
    {
        source : string
        replacement : string
    }

type replacement =
    {
        substitution : substitution
        location : int
    }

let parseSubstition (str:string) =
    let a = str.Replace(" => ",";").Split(';')
    {
        source  = a.[0]
        replacement = a.[1]
    }

let substitutions = input |> Array.rev |> Array.tail |> Array.tail |> Array.map parseSubstition
let sources = substitutions |> Array.map(fun x -> x.source) |> Array.distinct |> Array.sort

let nextLocation (patterns: string array) (seg:string)=
    let source = patterns |> Array.filter(fun s -> seg.IndexOf s <> -1) |> Array.minBy(fun s -> seg.IndexOf s)
    source, seg.IndexOf source

let hasSubstitutionLocation (patterns : string array )(seg : string)=
    patterns |> Array.filter(fun s -> seg.IndexOf s <> -1) |> Array.length > 0

let step (patterns : string array) ((seg,i) : string*int)  =
    if (hasSubstitutionLocation patterns seg) then
        let nextSource, idx = nextLocation patterns seg
        let nextSegment = seg.Substring(idx + nextSource.Length)
        Some ((nextSource, i+idx), (nextSegment,i+idx+nextSource.Length))
    else
        None

let locations = Seq.unfold (step sources) (molecule,0) |> Array.ofSeq

let replacements transformations locs= 
    transformations |> Array.collect(fun s -> locs |> Array.filter(fun l -> fst l = s.source) |> Array.map(fun l -> {substitution = s; location = snd l}) )

let replace (str: string) (rep: replacement)=
    str.Remove(rep.location, rep.substitution.source.Length).Insert(rep.location, rep.substitution.replacement)

let question1 = 
    replacements substitutions locations |> Array.map( replace molecule) |> Array.distinct |> Array.length

let collapses = substitutions |> Array.map(fun s -> {source = s.replacement; replacement = s.source})

let csrc = collapses |> Array.map(fun s -> s.source) |> Array.distinct 

let rec collapseLength (mol:string) =
    if( mol = "e") then
        0
    else
        let clocs = Seq.unfold (step csrc) (molecule,0) |> Array.ofSeq |> Array.sortByDescending(fun (x,y) -> x.Length)
        if(clocs.Length = 0) then 
            500
        else
            1 + (replacements collapses clocs |> Array.Parallel.map( replace mol) |> Array.Parallel.map collapseLength |> Array.min)


