let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\input\day05.txt")

let elements = 
    input.ToLower().ToCharArray() 
    |> Array.distinct
    |> Array.map string
    |> Array.toList

let reactants = 
    elements
    |> List.collect(fun s -> [s + s.ToUpper(); s.ToUpper() + s])

let react (polymer:string) =
    reactants |> List.fold(fun (s:string) t -> s.Replace(t,"")) polymer

let reducePolymer polymer = 
    let nextReducedLength s= 
        let red = react s
        if s.Length = red.Length then
            None
        else
            Some (red.Length,red)
    Seq.unfold nextReducedLength polymer
    |> Seq.last

let part1  = 
    reducePolymer input

let part2 = 
    elements 
    |> List.map (fun s -> input.Replace(s,"").Replace(s.ToUpper(),""))
    |> List.map reducePolymer
    |> List.min

//Shorter and (much) faster code using a stack
let reduce (s: int list) t  = 
    match s with
    | x::xs when abs (x - t) = 32 ->  xs 
    | _ -> t::s

let red2 (pol:string) = 
    pol.ToCharArray()
    |> Seq.map int
    |> Seq.fold reduce []
    |> List.length

red2 input
['a'..'z'] 
|> List.map (fun s -> input.Replace(string s,"").Replace((string s).ToUpper(),""))
|> List.map red2
|> List.min
