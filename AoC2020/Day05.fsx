let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt")

let seatId (boardingpass:string) =
    let binary = 
        boardingpass
            .Replace("F","0")
            .Replace("B","1")
            .Replace("L","0")
            .Replace("R","1")
    System.Convert.ToInt32(binary,2)

let part1 =  
    input
    |> Seq.map seatId
    |> Seq.max

let part2 = 
    input
    |> Seq.map seatId
    |> Seq.sort
    |> Seq.windowed 2
    |> Seq.map Seq.sum
    |> Seq.filter(fun a -> a%2=0)
    |> Seq.map (fun a -> a/2)
    |> Seq.exactlyOne

let part2_alternative = 
    let max = input |> Seq.map seatId |> Seq.max
    let min = input |> Seq.map seatId |> Seq.min
    let sum = input |> Seq.map seatId |> Seq.sum
    ((max+1)*max - (min-1)*min)/2 - sum