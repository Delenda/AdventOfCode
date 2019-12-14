let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day14.txt")

type amount = {chemical:string; quantity:int64}
type recipy = {amount:amount; compounds : amount list}
type recipies = Map<string, recipy>

let testRecipies1 = 
    [|
        "10 ORE => 10 A"
        "1 ORE => 1 B"
        "7 A, 1 B => 1 C"
        "7 A, 1 C => 1 D"
        "7 A, 1 D => 1 E"
        "7 A, 1 E => 1 FUEL"
    |]

let testRecipies2 = 
    [|
        "157 ORE => 5 NZVS"
        "165 ORE => 6 DCFZ"
        "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
        "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
        "179 ORE => 7 PSHF"
        "177 ORE => 5 HKGWZ"
        "7 DCFZ, 7 PSHF => 2 XJWVT"
        "165 ORE => 2 GPVTF"
        "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"
    |]

let testRecipies3 = 
    [|
        "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
        "17 NVRVD, 3 JNWZP => 8 VPVL"
        "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
        "22 VJHF, 37 MNCFX => 5 FWMGM"
        "139 ORE => 4 NVRVD"
        "144 ORE => 7 JNWZP"
        "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
        "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
        "145 ORE => 6 MNCFX"
        "1 NVRVD => 8 CXFTF"
        "1 VJHF, 6 MNCFX => 4 RFSQX"
        "176 ORE => 6 VJHF"
    |]

let testRecipies4 = 
    [|
        "171 ORE => 8 CNZTR"
        "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
        "114 ORE => 4 BHXH"
        "14 VRPVC => 6 BMBT"
        "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
        "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
        "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
        "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
        "5 BMBT => 4 WPTQ"
        "189 ORE => 9 KTJDG"
        "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
        "12 VRPVC, 27 CNZTR => 2 XDBXC"
        "15 KTJDG, 12 BHXH => 5 XCVML"
        "3 BHXH, 2 VRPVC => 7 MZWV"
        "121 ORE => 7 VRPVC"
        "7 XCVML => 6 RJRHP"
        "5 BHXH, 4 VRPVC => 5 LTCX"
    |]

let recipy (str:string) =
    let ingredients = seq{for m in System.Text.RegularExpressions.Regex("((\d+) (\w+))+").Matches str do
                            yield { chemical = m.Groups.[3].Value; quantity = m.Groups.[2].Value |> int64}}
    let result = ingredients |> Seq.last
    result.chemical, {amount = result; compounds = ingredients |> Seq.rev |> Seq.tail |> Seq.toList}

let productionStep (recipies:recipies) ((needed, have, ore): Map<string,int64>*Map<string,int64>*int64) =
    if needed.IsEmpty then
        None
    else
        let neededNonReduced = 
            needed
            |> Seq.map(fun need ->
                let chemical = need.Key
                let quantity = need.Value
                let recipy = recipies.[chemical]
                let productions =
                    if quantity%recipy.amount.quantity = 0L then
                        quantity/recipy.amount.quantity
                    else
                        1L + quantity/recipy.amount.quantity
                recipy.compounds |> List.map(fun amt -> amt.chemical, amt.quantity * productions))
            |> Seq.collect id
            |> Seq.groupBy fst
            |> Seq.map(fun (a,b) -> (a, b |> Seq.sumBy(fun d -> snd d)))
            |> Map

        let surplus = 
            needed
            |> Seq.map(fun need ->
                let chemical = need.Key
                let quantity = need.Value
                let recipy = recipies.[chemical]
                let surplus = 
                    if quantity%recipy.amount.quantity = 0L then
                        0L
                    else
                        (1L + quantity/recipy.amount.quantity)*recipy.amount.quantity - quantity
                chemical, surplus)
            |> Seq.groupBy fst
            |> Seq.map(fun (a,b) -> (a, b |> Seq.sumBy(fun d -> snd d)))
            |> Seq.filter(fun k -> snd k <> 0L)
            |> Map

        let haveAndSurplus = 
            surplus 
            |> Seq.fold(fun (s:Map<string,int64>) t -> 
                if s.ContainsKey t.Key then 
                    s.Add(t.Key, t.Value + s.[t.Key])
                else
                    s.Add(t.Key, t.Value)) have  

        let reductions = 
            Set.intersect (neededNonReduced |> Seq.map(fun k -> k.Key) |> Set) (haveAndSurplus |> Seq.map(fun k -> k.Key) |> Set)
            |> Seq.map(fun chemical -> chemical, min neededNonReduced.[chemical] haveAndSurplus.[chemical])
            |> Map

        let newNeeded = 
            neededNonReduced 
            |> Seq.filter(fun k -> k.Key <> "ORE")
            |> Seq.map(fun k-> 
                let reduction = if reductions.ContainsKey k.Key then reductions.[k.Key] else 0L
                k.Key, k.Value - reduction)
            |> Map

        let haveReduced =
            haveAndSurplus
            |> Seq.map(fun k -> 
                let reduction = if reductions.ContainsKey k.Key then reductions.[k.Key] else 0L
                k.Key, k.Value - reduction)
            |> Map

        let producedOre = 
            neededNonReduced 
            |> Seq.filter(fun k -> k.Key = "ORE") 
            |> Seq.sumBy(fun k -> k.Value)

        let totalOre = ore + producedOre

        Some(totalOre, (newNeeded, haveReduced, totalOre))

let neededOre (recipyStrings:string array) fuelQuantity= 
    let recipies = recipyStrings |> Array.map recipy |> Map
    Seq.unfold (productionStep recipies) (["FUEL", fuelQuantity] |> Map, Map.empty, 0L) |> Seq.last

let test1 = neededOre testRecipies1 1L= 31L
let test2 = neededOre testRecipies2 1L= 13312L
let test3 = neededOre testRecipies3 1L= 180697L 
let test4 = neededOre testRecipies4 1L= 2210736L 

let trillionOre = 1000000000000L

let test5 = (neededOre testRecipies2 82892753L < trillionOre && neededOre testRecipies2 (82892753L + 1L) > trillionOre )
let test6 = (neededOre testRecipies3  5586022L < trillionOre && neededOre testRecipies3 ( 5586022L + 1L) > trillionOre )
let test7 = (neededOre testRecipies4   460664L < trillionOre && neededOre testRecipies4 (  460664L + 1L) > trillionOre )

let rec bipartition recipyStrings upper lower = 
    let middle = (upper + lower)/2L
    if neededOre recipyStrings middle <= trillionOre && neededOre recipyStrings (middle + 1L) >  trillionOre then 
        middle
    else
        if neededOre recipyStrings middle < trillionOre then
            bipartition recipyStrings upper middle
        else
            bipartition recipyStrings middle lower

let part1 = neededOre input 1L
let part2 = bipartition input (2L*trillionOre/part1) (trillionOre/part1)