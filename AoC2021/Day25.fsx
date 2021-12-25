let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day25.txt") |> Array.map(fun s -> s.ToCharArray())

let maxY = input.Length
let maxX = input.[0].Length

let moveEast (herd:(char array) array) =
    herd
    |> Array.mapi(fun i line -> 
        line |> Array.mapi(fun j cucumber -> 
        if cucumber = '.' && line.[(j-1+maxX)%maxX] = '>' then '>' else
        if cucumber = '>' && line.[(j+1)%maxX] = '.' then '.' else
        cucumber))

let moveSouth (herd:(char array) array) =
    herd
    |> Array.mapi(fun i line -> 
        line |> Array.mapi(fun j cucumber -> 
        if cucumber = '.' && herd.[(i-1+maxY)%maxY].[j] = 'v' then 'v' else
        if cucumber = 'v' && herd.[(i+1)%maxY].[j] = '.' then '.' else
        cucumber))

let step = moveEast >> moveSouth

let part1 = 
    input
    |> Seq.unfold(fun s -> 
        let x = step s
        if s = x then None
        else Some(x,x))
    |> Seq.length
    |> (+) 1