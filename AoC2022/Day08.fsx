let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day08.txt") 

let trees = input |> Array.map(fun s -> s.ToCharArray() |> Array.map (string >> int))

let size = trees.Length-1

let search direction coordinates= 
    seq{for row in [0..size] do
        let found = 
            [0..size] |> direction
            |> List.fold(fun ((limit, found): int * Set<int*int>) column -> 
                let (r,c) = coordinates (row,column)
                let height = trees.[r].[c]
                let newLimit = max limit height
                if height > limit then
                    newLimit, (found.Add (r,c))
                else
                    newLimit, found
                ) (-1, Set.empty)
            |> snd
        yield found
            }
    |> Set.unionMany

let swap (a,b) = b,a

let part1 =
    let left  = search id       id
    let right = search List.rev id
    let down  = search id       swap
    let up    = search List.rev swap
    [left;right;down;up] |> Set.unionMany |> Set.count      

let lookup (row,column) = 
    let height = trees.[row].[column]
    let reach = 
        Seq.initInfinite(fun i -> row - 1 - i)
        |> Seq.takeWhile(fun i -> i >= 0)
        |> Seq.takeWhile(fun i -> trees.[i].[column] < height )
        |> Seq.length
    if reach < row then reach + 1 else reach

let lookleft (row,column) = 
    let height = trees.[row].[column]
    let reach = 
        Seq.initInfinite(fun i -> column - 1 - i)
        |> Seq.takeWhile(fun i -> i >= 0)
        |> Seq.takeWhile(fun i -> trees.[row].[i] < height )
        |> Seq.length
    if reach < column then reach + 1 else reach

let lookdown (row,column) =
    let height = trees.[row].[column]
    let reach = 
        Seq.initInfinite(fun i -> row + 1 + i)
        |> Seq.takeWhile(fun i -> i <= size)
        |> Seq.takeWhile(fun i -> trees.[i].[column] < height )
        |> Seq.length
    if reach < size - row then reach + 1 else reach


let lookright (row,column) =
    let height = trees.[row].[column]
    let reach = 
        Seq.initInfinite(fun i -> column + 1 + i)
        |> Seq.takeWhile(fun i -> i <= size)
        |> Seq.takeWhile(fun i -> trees.[row].[i] < height )
        |> Seq.length
    if reach < size - column then reach + 1 else reach


let part2 = 
    seq{for row in [0..size] do
        for col in [0..size] do
        let u = lookup(row,col)
        let d = lookdown(row,col)
        let l = lookleft(row,col)
        let r = lookright(row,col)
        yield u*d*l*r
        }
    |> Seq.max

