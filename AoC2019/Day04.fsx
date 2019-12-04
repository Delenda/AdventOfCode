let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day04.txt") 

let range = 
    input.Split('-')
    |> Array.map int
    |> fun s -> [s.[0]..s.[1]]

let part1 = 
    range
    |> Seq.map string
    |> Seq.map(fun s -> s.ToCharArray())
    |> Seq.filter(fun a -> 
        let pairs = a |> Array.windowed 2
        let increasing = pairs |> Seq.forall(fun pair -> pair.[0] <= pair.[1])
        let double     = pairs |> Seq.exists(fun pair -> pair.[0] =  pair.[1])
        increasing && double)
    |> Seq.length

let part2 =
    range
    |> Seq.map string
    |> Seq.map(fun s -> s.ToCharArray())
    |> Seq.filter(fun a -> 
        let pairs = a |> Array.windowed 2
        let increasing = pairs |> Seq.forall(fun pair -> pair.[0] <= pair.[1])
        let doubles    = pairs |> Seq.filter(fun pair -> pair.[0] =  pair.[1]) |> Seq.map(fun s -> s.[0])
        let originalString = a |> Seq.map string |> String.concat ""
        let nonTripletExists = 
            doubles
            |> Seq.map(fun c -> string c+ string c + string c )
            |> Seq.exists(fun triplet -> originalString.Contains(triplet) |> not)
        increasing && nonTripletExists)
    |> Seq.length
    