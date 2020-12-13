let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day13.txt")

let part1 = 
    let earliestTime = input.[0] |> uint64
    let id = 
        input.[1].Split(',')
        |> Array.filter(fun c -> c <> "x")
        |> Array.map uint64
        |> Seq.minBy(fun id -> id - (earliestTime%id))
    id * (id - (earliestTime%id))

// source :http://gettingsharper.de/2011/11/21/extended-euclidean-algorithm-in-f/
let euclid_algorithm (a : bigint) (b : bigint) =
    let rec inner (r'', s'', t'') (r', s', t') = 
        let step () = 
            let q = r'' / r'
            let r = r'' - q*r'
            let s = s'' - q*s'
            let t = t'' - q*t'
            (r, s, t)
        if r' = 0I then (r'', s'', t'')
        else inner (r', s', t') (step())
    inner (a, 1I, 0I) (b, 0I, 1I)

let inverse_Chinese_Remainder_Thm (r,p) (s,q) =
    let (d,a,b) = euclid_algorithm p q // d = a*p + b*q
    let composite = (r*b*q + s*a*p)%(p*q)
    if composite < 0I then 
        composite + p*q 
    else 
        composite

let part2 = 
    input.[1].Split(',') 
    |> Seq.indexed 
    |> Seq.filter(fun c -> snd c <> "x") 
    |> Seq.map(fun (idx, c) ->bigint idx, bigint (int c)) 
    |> Seq.fold (fun (m, idProducts) (idx, id) -> (inverse_Chinese_Remainder_Thm (m,idProducts) (-idx, id), idProducts*id)) (0I,1I)
    |> fst
    |> fun s -> s.ToString()