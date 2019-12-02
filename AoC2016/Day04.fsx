let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day04.txt")
let regex = System.Text.RegularExpressions.Regex "([\w-]+)-(\d+)\[(\w{5})\]"

type room = 
    { name : string; Id :int ; checksum:string}
    member this.CalculatedChecksum = 
        this.name.Replace("-","").ToCharArray()
        |> Seq.groupBy id
        |> Seq.sortBy(fun (a,b) -> -(b|> Seq.length), a)
        |> Seq.take 5
        |> Seq.map fst
        |> Seq.map string
        |> String.concat ""
    member this.DecryptedName = 
        this.name.Replace("-"," ").ToCharArray()
        |> Array.map(fun c -> 
            if c = ' ' then c else
            (int c - int 'a' + this.Id) % 26 + int 'a' |> char)
        |> Array.map string
        |> String.concat ""

let part1 =  
    input
    |> Seq.map(fun s -> regex.Match(s).Groups)
    |> Seq.map(fun g -> {name = g.[1].Value; Id = g.[2].Value |> int ; checksum = g.[3].Value})
    |> Seq.filter(fun x -> x.checksum = x.CalculatedChecksum)
    |> Seq.sumBy(fun x -> x.Id)

let part2 = 
    input
    |> Seq.map(fun s -> regex.Match(s).Groups)
    |> Seq.map(fun g -> {name = g.[1].Value; Id = g.[2].Value |> int ; checksum = g.[3].Value})
    |> Seq.filter(fun x -> x.checksum = x.CalculatedChecksum)
    |> Seq.filter(fun x -> x.DecryptedName.Contains "northpole")
    |> Seq.map(fun x -> x.Id)
    |> Seq.exactlyOne