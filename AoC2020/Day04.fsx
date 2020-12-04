let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day04.txt")

type passport =
    {
        CountryId : int option
        BirthYear : int
        EyeColor : string
        Height : string
        PassportId : string
        IssueYear : int
        ExpirationYear : int
        HairColor : string
    }

let parse str =
    let pattern = @"(ecl|pid|iyr|hcl|byr|hgt|eyr|cid):(\S+)"
    let regex = System.Text.RegularExpressions.Regex pattern
    let fields =
        seq { for m in regex.Matches str do
                yield (m.Groups.[1].Value, m.Groups.[2].Value)}
        |> Map

    let value key =
        if fields.ContainsKey key then fields.[key] else failwith (sprintf "Unexpected key: %s" key)

    match fields.Count, fields.ContainsKey "cid" with
    | 7, false
    | 8, true ->
        {
            CountryId = fields.TryFind "cid" |> Option.map int
            BirthYear = value "byr" |> int
            EyeColor = value "ecl"
            Height = value "hgt"
            PassportId = value "pid"
            IssueYear = value "iyr" |> int
            ExpirationYear = value "eyr" |> int
            HairColor = value "hcl"
        } |> Some
    | _ -> None

let passports =
    input
        |> Array.fold(fun s t ->
            if t = "" then ((snd s)::fst s, "") else (fst s , (snd s) + " " + t)
            ) ([], "")
        |> fun (a,b) -> b::a
    |> List.map parse

open System.Text.RegularExpressions

let validation pp =
    if pp.BirthYear > 2002 || pp.BirthYear < 1920 then false else
    if pp.IssueYear < 2010 || pp.IssueYear > 2020 then false else
    if pp.ExpirationYear < 2020 || pp.ExpirationYear > 2030 then false else
    if Regex.Match(pp.Height, @"\d+[in|cm]").Success |> not then false else
    let ht = int(pp.Height.Replace("cm","").Replace("in",""))
    if pp.Height.EndsWith "cm" && (ht < 150 || ht > 193) then false else
    if pp.Height.EndsWith "in" && (ht < 59 || ht > 76) then false else
    if (pp.Height.EndsWith "in" || pp.Height.EndsWith "cm") |> not then false else
    if Regex.Match(pp.HairColor, @"#[\d|a-f]{6}$").Success |> not then false else
    if Regex.Match(pp.EyeColor, @"[amb|blu|brn|gry|grn|hzl|oth]$").Success |> not then false else
    if Regex.Match(pp.PassportId, @"\d{9}$").Success |> not then false else
    true

let part1 =
    passports
    |> List.filter(fun p -> p.IsSome)
    |> List.length

let part2 =
    passports
    |> List.filter(fun p -> p.IsSome)
    |> List.filter (Option.map validation >> Option.defaultValue false >> not)
    |> List.length