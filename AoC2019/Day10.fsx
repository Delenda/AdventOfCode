let astMap = 
    System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day10.txt")
    |> Array.map(fun (s:string) -> s.ToCharArray())

let rec gcd a b = 
    let x = max (abs a) (abs b)
    let y = min (abs a) (abs b)
    let r = x%y
    if r = 0 then y else gcd y r

let directions (map: char array array) y x =
    seq{
        yield (1,0)
        yield (-1,0)
        yield (0,1)
        yield (0,-1)
        for a in [0..(map.Length)-1] do
        for b in [0..(map.[0].Length)-1] do
            if b<>y && x<>a  then 
                let d = gcd (a-x) (b-y)
                yield (b-y)/d, (a-x)/d}
    |> Seq.distinct

let ray (map: char array array) y x (a,b) = 
    Seq.initInfinite(fun i -> y + i*a, x + i*b)
    |> Seq.tail
    |> Seq.takeWhile(fun (i,j) -> i < map.Length && j < (map.[0].Length) && 0 <= i && 0 <= j)
    |> Seq.filter(fun (i,j) -> map.[i].[j] = '#')

let lineSearch (map: char array array) y x (a,b) = 
    ray map y x (a,b)
    |> Seq.isEmpty
    |> not

let count map y x = 
    directions map y x 
    |> Seq.filter (lineSearch map y x)
    |> Seq.length

let maximalLocation (map:char array array) =
    map 
    |> Seq.mapi(fun i row -> i,row |> Seq.mapi(fun j c-> j, if c = '#' then count map i j else 0 ) |> Seq.maxBy(snd))
    |> Seq.maxBy(snd >> snd)
    
let part1 = maximalLocation astMap |> (snd >> snd)
    
let part2 = 
    let station_y, station_x = maximalLocation astMap |> fun( a, (b,c)) -> a,b

    let directions_positive_x = directions astMap station_y station_x |> Seq.filter(fun (y,x) -> x > 0) |> Seq.sortBy(fun (y,x) -> (float y)/(float x))
    let directions_negative_x = directions astMap station_y station_x |> Seq.filter(fun (y,x) -> x < 0) |> Seq.sortBy(fun (y,x) -> (float y)/(float x))

    //Answer in part1 was greater than 200, so no need to handle several rotations, and no need to handle less than 200 directions
    let direction = 
        seq{ yield (-1,0)
             for direction in directions_positive_x do yield direction
             yield (1,0)
             for direction in directions_negative_x do yield direction} 
        |> Seq.filter(lineSearch astMap station_y station_x)
        |> Seq.skip 199
        |> Seq.head

    ray astMap station_y station_x direction
    |> Seq.head
    |> fun (y,x) -> 100*x + y