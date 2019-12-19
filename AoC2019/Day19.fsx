let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ +   @"\Input\Day19.txt") 

#load "IntCode.fsx"

let part1 = 
    Array.init 50 id
    |> Array.map(fun y -> 
        Array.init 50 id 
        |> Array.map(fun x -> IntCode.runProgram input [int64 x; int64 y] |> List.head ))
    |> Array.map(Array.sum)
    |> Array.sum


let part2 = 
    let x_lower_at_y500 = 
        seq{for i in [0..499] do yield i, IntCode.runProgram input [int64 i; 500L] |> List.head}
        |> Seq.skipWhile(fun x -> snd x = 0L)
        |> Seq.head
        |> fst

    let x_upper_at_y500 = 
        Seq.initInfinite(fun i -> x_lower_at_y500 + i)
        |> Seq.map(fun i -> i,IntCode.runProgram input [int64 i; 500L] |> List.head)
        |> Seq.takeWhile(fun x -> snd x = 1L)
        |> Seq.last
        |> fst

    let lowerSlope = float 500/float x_lower_at_y500
    let upperSlope = float 500/float x_upper_at_y500

    //So for lower left corner (x,y) of 100x100 square we have (approximately):
    //lowerSlope * x = y
    //upperSlope * (x + 99) = y - 99
    //The solution to these equations is : x = 99 * (1 + upperSlope)/(lowerSlope - upperSlope)

    let  x_lower = 99.0 * (1.0 + upperSlope) /(lowerSlope - upperSlope) |> int64

    let beam x y =
        IntCode.runProgram input [int64 x; int64 y] |> List.head |> int
    
    let isUpperEdge x y =
        (beam x y = 1) && (beam x (y-1) = 0) && (beam (x+1) y = 0)

    let y_lower = lowerSlope * (float x_lower) |> int64

    [(y_lower - 25L)..(y_lower + 25L)]
    |> Seq.map(fun y -> 
        seq{for i in [(x_lower - 50L)..(x_lower + 1000L)] do yield i, IntCode.runProgram input [int64 i; y] |> List.head}
        |> Seq.skipWhile(fun x -> snd x = 0L)
        |> Seq.head
        |> fst, y)
    |> Seq.filter(fun (x,y) -> isUpperEdge (int x+99) (int y - 99))
    |> Seq.head
    |> fun (x,y) -> 10000L*x + (y - 99L)