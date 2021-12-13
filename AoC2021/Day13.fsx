let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day13.txt") 

let inputDots = 
    input
    |> Array.filter(System.String.IsNullOrEmpty >> not)
    |> Array.filter(fun s -> s.[0] |> System.Char.IsDigit)
    |> Array.map(fun s -> 
        let f = s.Split(',')
        int f.[0], int f.[1] )
    |> Set

type dot = int*int
type fold = | AlongX of int | AlongY of int

let foldInstructions = 
    input
    |> Array.filter(System.String.IsNullOrEmpty >> not)
    |> Array.filter(fun s -> s.[0] |> System.Char.IsLetter)
    |> Array.map(fun s -> 
        let f = s.Split('=')
        let coord = f.[1] |> int
        match f.[0] |> Seq.last with
        | 'x' -> AlongX coord
        | 'y' -> AlongY coord
        | _ -> "unexpected char" |> failwith)

let fold dots foldInstruction=
    match foldInstruction with
    | AlongX coord -> dots |> Set.map(fun (x,y) -> min x (2*coord - x), y)
    | AlongY coord -> dots |> Set.map(fun (x,y) -> x, min y (2*coord - y))

let printDots (dots:Set<dot>) =
    let minX = dots |> Set.map fst |> Seq.min
    let maxX = dots |> Set.map fst |> Seq.max
    let minY = dots |> Set.map snd |> Seq.min
    let maxY = dots |> Set.map snd |> Seq.max
    [minY-1..maxY] 
    |> List.map(fun y -> 
         [minX..maxX] 
         |> List.map(fun x -> if dots.Contains (x,y) then "#" else " ") 
         |> String.concat "")
    |> String.concat "\r\n"

let part1 = fold inputDots foldInstructions.[0]  |> Set.count
let part2 = 
    foldInstructions
    |> Seq.fold fold inputDots
    |> printDots