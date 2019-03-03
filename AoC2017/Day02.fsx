let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day02.txt")

let rows = input |> Array.map(fun s -> s.Split('\t')) |> Array.map( Array.map int )

let question1 = 
    let maxs = rows |> Array.map(Array.max)
    let mins = rows |> Array.map(Array.min)
    Array.zip maxs mins |> Array.sumBy(fun (x,y) -> x-y)

let question2 = 
    let pairs = rows |> Array.collect(fun row -> row |> Array.collect(fun x -> row |> Array.map(fun y -> (x,y))))
    pairs
        |> Array.filter(fun (x,y) -> x<>y)
        |> Array.filter(fun (x,y) -> x%y = 0)
        |> Array.map(fun (x,y) -> x/y)
        |> Array.sum