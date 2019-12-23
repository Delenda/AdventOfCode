let input = System.IO.File.ReadAllLines(@"D:\Src\Aoc2015\Input\Day17.txt")

let rec antalKombi target candidates=
    match (candidates |> List.length), target with
    | 0, 0 -> 1
    | 0, _ -> 0
    | _, _ -> antalKombi (target - candidates.Head) candidates.Tail + antalKombi target candidates.Tail

let rec antalMinimalKombi target candidates=
    match (candidates |> List.length), target with
    | 0, 0 -> 0,1
    | 0, _ -> System.Int32.MaxValue - 1,0
    | _, _ ->
        let hLength, hNumber = antalMinimalKombi (target - candidates.Head) candidates.Tail
        let tLength, tNumber = antalMinimalKombi target candidates.Tail
        if( hLength + 1 < tLength) then
            hLength + 1, hNumber
        else if (hLength + 1 > tLength) then
            tLength, tNumber
        else
            tLength, hNumber + tNumber

let containers = input |> Array.map int |> List.ofArray

let question1 = antalKombi 150 containers
let question2 = antalMinimalKombi 150 containers |> snd

let binom n k = antalKombi k (List.replicate n 1)

let binoms n =
    [0..n] |> List.map(binom n)

[2..2..28] |> List.map (fun i -> binom i (i/2))