let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day11.txt") 

let createCave (lines:string array) = 
    lines
    |> Array.map(fun s -> s.ToCharArray() |> Array.map string |> Array.map int)
    |> Array.mapi(fun i s -> s |> Array.mapi(fun j e -> (i,j), e))
    |> Array.collect id
    |> Map

let octopuses = createCave input
    
let example = 
    [|"5483143223"
      "2745854711"
      "5264556173"
      "6141336146"
      "6357385478"
      "4167524645"
      "2176841721"
      "6882881134"
      "4846848554"
      "5283751526"|] |> createCave

let printCave (cave:Map<int*int,int>) = 
    System.Console.WriteLine ""
    seq{for i in [0..9] do
        System.Console.WriteLine ""
        for j in [0..9] do
        System.Console.Write (cave.[i,j])}
    |> Seq.iter id

let adjacentLocations (i,j) = 
    seq{for di in [-1;0;1] do
        for dj in [-1;0;1] do
        yield i+di,j+dj} //no need to filter on bounds or delta = 0
    |> Set
   
let step cave = 
    let newCave = cave |> Map.map(fun location energy -> energy + 1)
    let newOct = 
        newCave
        |> Seq.unfold(fun oct -> 
            let flashLocations = oct |> Map.filter(fun _ energy -> energy > 9) |> Map.toSeq |> Seq.map fst |> Set
            if flashLocations.IsEmpty then None else
            let newoct = 
                oct 
                |> Map.map(fun location energy -> 
                    if flashLocations.Contains location || energy = 0 then 
                        0 
                    else 
                        let dEnergy = Set.intersect (adjacentLocations location) flashLocations |> Set.count
                        energy + dEnergy)
            Some(newoct, newoct)) 
    let s = 
        if newOct |> Seq.isEmpty then newCave else newOct |> Seq.last
    Some(s,s)

let part1 = 
    Seq.unfold step octopuses
    |> Seq.take 100
    |> Seq.sumBy(Map.filter(fun _ energy -> energy = 0) >> Map.count)
   
let part2 = 
    Seq.unfold step octopuses
    |> Seq.takeWhile(Map.toSeq >> Seq.map snd >> Seq.exists((<>) 0))
    |> Seq.length
    |> (+) 1