let input = System.IO.File.ReadAllLines(@"C:\Users\Lars\source\repos\AoC2018\Input\Day10.txt")

type star = {x:int; y:int; vx:int;vy:int}

let parseStar (s:string) = 
    let fields  = s.Replace("position=<","").Replace("> velocity=<",";").Replace(">",";").Replace(",",";").Replace(" ","").Split(';')
    let x = fields.[0] |> int
    let y = fields.[1] |> int
    let vx = fields.[2] |> int
    let vy = fields.[3] |> int
    {x = x; y = y; vx = vx; vy = vy}

let inputStars = input |> Array.map parseStar

let moveStar (steps:int) (s:star) =
    {x = s.x + s.vx*steps; y = s.y + s.vy*steps; vx = s.vx; vy = s.vy}

let size (stars :star array) =
    let maxX = stars |> Array.maxBy (fun s -> s.x) |> fun s -> s.x
    let minX = stars |> Array.minBy (fun s -> s.x) |> fun s -> s.x
    let maxY = stars |> Array.maxBy (fun s -> s.y) |> fun s -> s.y
    let minY = stars |> Array.minBy (fun s -> s.y) |> fun s -> s.y
    maxX + maxY - minX - minY

let renderStars (stars : star array) =
    let maxX = stars |> Array.maxBy (fun s -> s.x) |> fun s -> s.x
    let minX = stars |> Array.minBy (fun s -> s.x) |> fun s -> s.x
    let maxY = stars |> Array.maxBy (fun s -> s.y) |> fun s -> s.y
    let minY = stars |> Array.minBy (fun s -> s.y) |> fun s -> s.y
    stars 
    |> Array.map(fun s -> (s.x - minX, s.y - minY))
    |> Array.groupBy snd
    |> Array.sortBy fst
    |> Array.map(fun (x,y) -> 
                    let sj = y |> Array.map fst
                    [0..(maxX-minX)] |> List.map(fun i -> if Array.contains i sj then "#" else "." ) |> Array.ofList |> String.concat "")
    |> String.concat "\r\n"
    |> fun s -> "\r\n"+s
// search stars
Seq.unfold(fun s -> 
               let sz = renderStars s
               let news = s |> Array.map (moveStar 1)
               Some(sz,news)) (inputStars |> Array.map (moveStar 10511))
|> Seq.take 3
|> List.ofSeq

let part2 = 
    Seq.unfold(fun s -> 
                   let sz = size s 
                   let news = s |> Array.map (moveStar 1)
                   Some(sz,news)) inputStars
    |> Seq.takeWhile(fun sz -> sz > 70)
    |> Seq.length

let part1 = 
    renderStars (inputStars |> Array.map (moveStar part2))
    