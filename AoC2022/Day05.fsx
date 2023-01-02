let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt") 
let crates = input |> Array.takeWhile(fun line -> line <> "") |> Array.rev |> Array.tail |> Array.rev
let moves = input |> Array.rev |> Array.takeWhile(fun line -> line <> "") |> Array.rev

let initialStacks  =
    let emptyStacks : Map<int,string list> = [1..9] |> List.map(fun d -> d,[]) |> Map
    crates
    |> Array.rev
    |> Array.fold(fun (st:Map<int,string list>) line -> 
        [1..9] 
        |> List.map(fun stack -> (stack-1)*4 + 1) 
        |> List.mapi(fun i d ->i+1, line.Substring(d,1))
        |> List.fold(fun (s:Map<int,string list>) (i,c) -> 
            if c <> " " then 
                s.Add(i,c::s.[i])
            else
                s) st) emptyStacks

let parseMove line = 
    let pattern = @"move (\d+) from (\d+) to (\d+)"
    let regex = System.Text.RegularExpressions.Regex(pattern)
    let m = regex.Match line
    let cnt = m.Groups.[1].Value |> int
    let src = m.Groups.[2].Value |> int
    let dst = m.Groups.[3].Value |> int
    cnt,src,dst

let craneMover order (stack:Map<int,string list>) (cnt,src,dst) = 
    let moves, remains = stack.[src] |> List.splitAt cnt
    let newDest = (order moves)@stack.[dst]
    stack.Add(src, remains).Add(dst,newDest)

let craneMover9000 = craneMover List.rev
let craneMover9001 = craneMover id
    
let move crane = 
    moves 
    |> Array.map parseMove
    |> Array.fold crane initialStacks
    |> Map.map(fun k v -> v.Head)
    |> Map.toSeq
    |> Seq.sort
    |> Seq.map snd
    |> String.concat ""

let part1 = move craneMover9000
let part2 = move craneMover9001