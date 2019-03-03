let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day11.txt")
type state = {x : double ; y : double; dist : double}

let dist (x :double) (y:double) =
    let xa = System.Math.Abs x
    let ya = System.Math.Abs y
    match xa < 2.0 * ya with
    | true ->  (ya - 0.5 * xa) + xa
    | false -> xa

let step distFunc {x = x; y = y; dist = d} str=
    let (xn,yn) =
        match str with
        | "n"  -> (x,       y + 1.0)
        | "nw" -> (x - 1.0, y + 0.5)
        | "ne" -> (x + 1.0, y + 0.5)
        | "s"  -> (x,       y - 1.0)
        | "sw" -> (x - 1.0, y - 0.5)
        | "se" -> (x + 1.0, y - 0.5)
        |_ -> failwith("unexpected direction: " + str)
    {x = xn; y = yn; dist = dist xn yn |> distFunc d}

let genericAnswer distFunc = 
    input 
        |> Array.map(fun s -> s.Split(',')) 
        |> Array.exactlyOne
        |> Array.fold (step distFunc)  {x = 0.0; y=0.0; dist = 0.0}
        |> fun s -> s.dist
        |> int

let question1 = genericAnswer (fun a b -> b)
let question2 = genericAnswer max