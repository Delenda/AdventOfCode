let input = System.IO.File.ReadAllLines(@"D:\Src\Aoc2015\Input\Day16.txt")

let m op a b=
    match a, b with
    | None, _ 
    | _, None -> true
    | Some a, Some b -> op a b

type characteristic =
    {
        Number : int
        children : int option
        cats : int option
        samoyeds : int option
        pomeranians : int option
        akitas : int option
        vizslas : int option
        goldfish : int option
        trees : int option
        cars : int option
        perfumes : int option
    }
    member this.m1  (c : characteristic) =
        let op = (=)
        m op this.children c.children
        && m op this.cats c.cats
        && m op this.samoyeds c.samoyeds
        && m op this.pomeranians c.pomeranians
        && m op this.akitas c.akitas
        && m op this.vizslas c.vizslas
        && m op this.goldfish c.goldfish
        && m op this.trees c.trees
        && m op this.cars c.cars
        && m op this.perfumes c.perfumes
    member this.m2  (c : characteristic) =
        m (=) this.children c.children
        && m (<) this.cats c.cats
        && m (=) this.samoyeds c.samoyeds
        && m (>) this.pomeranians c.pomeranians
        && m (=) this.akitas c.akitas
        && m (=) this.vizslas c.vizslas
        && m (>) this.goldfish c.goldfish
        && m (<) this.trees c.trees
        && m (=) this.cars c.cars
        && m (=) this.perfumes c.perfumes

let criteria = 
    {
        Number = -1
        children = Some 3
        cats = Some 7
        samoyeds = Some 2
        pomeranians = Some 3
        akitas = Some 0
        vizslas = Some 0
        trees = Some 3
        cars = Some 2
        perfumes = Some 1
        goldfish = Some 5
    }

let findChars charac str =
    let m = System.Text.RegularExpressions.Regex.Match(str, charac+ ": ([0-9]+)")
    if m.Success then Some (List.tail [for g in m.Groups -> g.Value] |> List.head |> int)
    else None

let parseCharacteristics (str:string)=
    let number = str.Split(':').[0].Replace("Sue ","") |> int
    {
        Number = number
        children = findChars "children" str
        cats = findChars "cats" str
        samoyeds = findChars "samoyeds" str
        pomeranians = findChars "pomeranians" str
        akitas = findChars "akitas" str
        vizslas = findChars "vizslas" str
        trees = findChars "trees" str
        cars = findChars "cars" str
        perfumes = findChars "perfumes" str
        goldfish = findChars "goldfish" str
    }

let characteristics = input |> Array.map parseCharacteristics

let question filter = 
    characteristics |> Array.filter filter |> Array.exactlyOne |> fun x -> x.Number

let question1 = question criteria.m1
let question2 = question criteria.m2