let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\Day18.txt")

let area =  
    input 
    |> Array.map(fun s -> s.ToCharArray())

let neighbours (a,b) =
    [-1..1] 
    |> List.collect(fun i -> [-1..1] |> List.map(fun j -> (a+i,b+j) ))
    |> List.filter(fun (x,y) -> 0<=x && 0<=y && x<=49 && y<= 49)
    |> List.filter(fun (x,y) -> x<> a || y<>b)
    |> Array.ofList

let adjacents = 
    [0..49] 
    |> Array.ofList
    |> Array.map(fun i -> [0..49] |> Array.ofList |> Array.map(fun j -> neighbours (i,j) ))

let newChar c adjs =
    if c = '.' then
        if adjs |> Array.filter(fun ch -> ch = '|') |> Array.length >= 3 then
            '|'
        else    
            '.'
    else if c = '|' then
        if adjs |> Array.filter(fun ch -> ch = '#') |> Array.length >= 3 then
            '#'
        else
            '|'
    else 
        if adjs |> Array.groupBy id |> Array.filter(fun x -> fst x <> '.') |> Array.length = 2 then
            '#'
        else
            '.'
        
let unfolder (s: char array array) =
    let news = 
        s
        |> Array.mapi(fun i l -> 
                        l |> Array.mapi(fun j c -> 
                            let adjs = 
                                adjacents.[i].[j] |> Array.map(fun (x,y) -> s.[x].[y])
                            newChar c adjs
                        ))
    Some(news,news)

let areaStr a = 
    a  |> Array.map(fun l -> l |> Array.map string |> String.concat "") |> String.concat "\r\n"

let printArea (a: char array array) = 
    System.Console.WriteLine ""
    System.Console.WriteLine (areaStr a)

let resourceValue s = 
    let grps = 
        s
        |> Array.collect id
        |> Array.groupBy id

    let wood = grps |> Array.filter(fun x -> fst x = '|') |> Array.exactlyOne |> snd |> Array.length
    let yard = grps |> Array.filter(fun x -> fst x = '#') |> Array.exactlyOne |> snd |> Array.length

    wood*yard

let part1 =
    let a = 
        Seq.unfold unfolder area
        |> Seq.take 10
        |> Seq.last
    resourceValue a

let unfolder2 ((s, t):(char array array)*(Set<string>)) =
    if t.IsEmpty then None else
    let news = 
        Seq.unfold unfolder s
        |> Seq.take 1
        |> Seq.exactlyOne
    let str = areaStr news
    let newt = 
        if t.Contains str then Set.empty else
            t.Add str
    let q = (news, newt)
    Some(news,q)

let part2 = 
    let firstRepeat = 
        Seq.unfold unfolder2 (area,Set.empty.Add "ghg")
        |> Seq.last
        |> areaStr

    let nodes = 
        Seq.unfold unfolder2 (area, Set.empty.Add "ghg")
        |> Seq.indexed
        |> Seq.filter(fun x -> (snd x |> areaStr) = firstRepeat)
        |> Seq.take 2
        |> List.ofSeq
        |> List.map fst

    let period = nodes.Tail.Head - nodes.Head
    let remainder = (1000000000 - nodes.Head)%period

    let final =
         Seq.unfold unfolder2 (area, Set.empty.Add "ghg")
         |> Seq.take (nodes.Head + remainder)
         |> Seq.last

    resourceValue final