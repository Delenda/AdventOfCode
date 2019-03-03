let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day06.txt")

let points = input |> Array.map(fun s -> s.Replace(" ","").Split(',')) |> Array.map(fun x -> (x.[0] |>int, x.[1] |> int))

let leftMost = points |> Array.minBy fst |> fst
let rightMost = points |> Array.maxBy fst |> fst
let upperMost = points |> Array.maxBy snd  |> snd
let lowerMost = points |> Array.minBy snd |> snd

let dist (x,y) (z,w) =
    abs(x-z) + abs (y-w)

let totaldist x = 
    points |> Array.sumBy(dist x)

let bounded = [leftMost..rightMost] |> List.collect(fun x -> [lowerMost..upperMost] |> List.map(fun y -> (x,y)))

let part1 = 
    bounded
    |> List.map(fun x -> 
                    let next = 
                        points 
                        |> Array.groupBy(fun y -> dist x y)
                        |> Array.minBy fst
                        |> snd
                    match next |> Array.length with
                    | 1 -> next |> Array.head
                    | _ -> (-1,-1)
                        )
    |> List.filter(fun x -> x <> (-1,-1)) 
    |> List.groupBy id 
    |> List.map snd 
    |> List.maxBy (fun x -> x |> List.length) 
    |> List.length

let part2 = 
    bounded
    |> List.map(fun x -> totaldist x)
    |> List.filter(fun x -> x < 10000)
    |> List.length