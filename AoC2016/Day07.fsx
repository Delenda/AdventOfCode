let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day07.txt") 

let hasABBA (s:string) =
    let regex = System.Text.RegularExpressions.Regex "(\w)(\w)\2\1"
    let m = regex.Match s
    if m.Success then
        m.Groups.[1].Value <> m.Groups.[2].Value
    else false

let hypersequences (s:string) =
    let regex = System.Text.RegularExpressions.Regex "(\[\w+\])+"
    seq{for m in regex.Matches(s) do yield m.Groups.[1].Value} 

let supports_TLS (s:string) = 
    let hs = hypersequences s
    let valid_hs = hs |> Seq.forall(hasABBA >> not)
    let outside = hs |> Seq.fold(fun (state:string) hseq -> state.Replace(hseq, ";")) s
    valid_hs && hasABBA outside

let part1 = 
    input
    |> Seq.filter supports_TLS
    |> Seq.length

let babs (s:string) = 
    s.ToCharArray()
    |> Seq.windowed 3
    |> Seq.filter(fun t -> t.[0] = t.[2] && t.[0] <> t.[1])
    |> Seq.map (fun t -> (string t.[1]) + (string t.[0]) + (string t.[1]))

let supports_SSL (s:string) =
    let hs = hypersequences s
    let outside = hs |> Seq.fold(fun (state:string) hseq -> state.Replace(hseq, ";")) s
    let bab_seq = babs outside
    seq{for hseq in hs do
        for bab in bab_seq do
        yield hseq, bab}
    |> Seq.exists(fun (hseq,bab) -> hseq.Contains bab)

let part2 =
    input
    |> Seq.filter supports_SSL
    |> Seq.length