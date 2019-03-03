let inputsn = 5177

let powerlevel serialnumber (x,y) =
    let rackid = x + 10
    let powerlevel = ((rackid * y + serialnumber)*rackid/100)%10 - 5
    powerlevel

let sqPowerlevel sn size (x,y) =
    [0..(size-1)] 
    |> List.collect(fun j -> [0..(size-1)] |> List.map(fun i -> powerlevel sn (x+i, y+j)))
    |> List.sum

let moveRight sn sz pl (i,j) =
    let leftCol = [j..(j+sz-1)] |> List.sumBy(fun k -> powerlevel sn (i,k))
    let rightCol = [j..(j+sz-1)]|> List.sumBy(fun k -> powerlevel sn (i+sz,k))
    pl-leftCol+rightCol

let moveDown sn sz pl (i,j)= 
    let topRow = [i..(i+sz-1)] |> List.sumBy(fun k -> powerlevel sn (k,j))
    let bottomRow = [i..(i+sz-1)] |> List.sumBy(fun k -> powerlevel sn (k, j+sz))
    pl-topRow+bottomRow

let maxSq sn sz = 
    let pl11 = sqPowerlevel sn sz (1,1)
    let topRow = 
        [2..(301-sz)] 
        |> List.scan(fun pl i-> moveRight sn sz pl (i-1,1)) pl11 
    let rows = 
        [2..(301-sz)] 
        |> List.scan(fun s j -> 
                        s |> List.mapi(fun i pl -> moveDown sn sz pl (i+1,j-1)) 
                        
                        ) topRow
    rows
    |> List.mapi(fun i t -> t |> List.mapi(fun j pl-> ((j+1,i+1),pl)))
    |> List.collect id
    |> List.maxBy snd

let maxSqDial sn =
    [1..300]
    |> List.map(fun i ->
                        System.Console.WriteLine i 
                        (i, maxSq sn i))
    |> List.maxBy (snd>>snd)
    |> fun res -> sprintf "%d,%d,%d" (snd res |> fst |> fst) (snd res |> fst |> snd) (fst res)
    
let part1 = 
    let res = maxSq inputsn 3 |> fst
    sprintf "%d,%d" (fst res) (snd res)
    
//let part2 = 
//    maxSqDial inputsn
 
// Optimized part 2
// According to : https://en.wikipedia.org/wiki/Summed-area_table
let grids sn = 
    let diags = 
        [1..300]
        |> List.collect(fun i -> [1..300] |> List.map(fun j -> (i,j)))
        |> List.groupBy(fun (x,y) -> x + y)
        |> List.sortBy fst
        |> List.collect snd
    let gridFold sn (s : Map<int*int,int>)(x,y) =
        let a = s.TryFind (x-1,y) |> Option.defaultValue 0
        let b = s.TryFind (x,y-1) |> Option.defaultValue 0
        let c = s.TryFind (x-1,y-1) |> Option.defaultValue 0
        let pl = powerlevel sn (x,y)
        s.Add((x,y), pl + a + b - c)
    let g = 
        diags
        |> List.fold (gridFold sn) (Map<int*int,int>([]))
    Array.replicate 300 (Array.replicate 300 0)
    |> Array.mapi(fun i d -> d |> Array.mapi(fun j e-> g.[(i+1,j+1)]))

let sqpl (grid: int array array) sz (x,y) =
    let getvalue a b =
        if a > 300 || b > 300 || a < 1 || b < 1 then 0 else grid.[a-1].[b-1] 
    let a = getvalue (x+sz-1) (y+sz-1)
    let b = getvalue (x+sz-1) (y-1)
    let c = getvalue (x-1) (y+sz-1)
    let d = getvalue (x-1) (y-1)
    a - b - c + d

let maxSqGrid grid sz =
    [1..(301-sz)]
    |> List.collect(fun i -> [1..(301-sz)] |> List.map(fun j -> (i,j)))
    |> List.map(fun k -> (k,sqpl grid sz k))
    |> List.maxBy snd

let maxSqDialGrid grid =
    [1..300]
    |> List.map(fun i ->(i, maxSqGrid grid i))
    |> List.maxBy (snd>>snd)
    |> fun res -> sprintf "%d,%d,%d" (snd res |> fst |> fst) (snd res |> fst |> snd) (fst res)

let part2_optimized = 
    (grids >> maxSqDialGrid) inputsn


//AOC 2016 Day19:
let aoc2016_19_part1 = 
    let a = System.Convert.ToString(3012210,2).ToCharArray() |> List.ofArray
    a.Tail @[a.Head]
    |> List.rev
    |> List.mapi(fun i b -> ((string >> int) b) * (pown 2 i))
    |> List.sum

let rec jos n =                             
    if n = 1 then 1
    else if n%6 = 0 then 3*(jos (n/3))
    else 
        let x = jos (n-1)               // i    a*     i    a       a   b*
        if x = n-1 then                 // h    b      h    b*      i   c
            1                           // g    c  =>  g    c  =    h   d
        else if x = (n-1)/2 then        // f    d      f    d       g   f
            n/2 + 1 + (n%2)             //    e          (x)
        else if x > (n-1)/2 then
            x + 2 - (n%2)               //  h   a*      h   a      a   b*
        else                            //  g   b       g   b*     h   c
            x + 1                       //  f   c   =>  f   c   =  g   d
                                        //  e   d      (x)  d        f
                  
let aoc2016_19_part2 = 
    jos  3012210    
