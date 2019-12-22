let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +   @"\Input\Day22.txt") 

type technique =
    | NewStack
    | Cut of int64
    | Increment of int64

let parse (s:string) =
    let m1 = System.Text.RegularExpressions.Regex("deal into new stack").Match s 
    let m2 = System.Text.RegularExpressions.Regex("cut (-?\d+)").Match s
    let m3 = System.Text.RegularExpressions.Regex("deal with increment (\d+)").Match s
    if m1.Success then
        NewStack
    else if m2.Success then
        Cut(m2.Groups.[1].Value |> int64)
    else
        Increment(m3.Groups.[1].Value |> int64)

let move (n:int64) (idx:int64) (technique:technique) =
    match technique with
    | NewStack -> n - idx - 1L
    | Cut cut -> (idx - cut + n)%n
    | Increment inc -> ((inc*idx)%n + n)%n

let shuffleCard (techniques:technique array) n idx=
     techniques
     |> Array.fold (move n) idx

let part1 = 
    shuffleCard (input |> Array.map parse) 10007L 2019L 

// https://rosettacode.org/wiki/Modular_inverse#F.23
let ModularMultiplicativeInverse n x =
  let rec fN n i x r l a =
    match r with
    | 0L -> x
    | _ -> let o = n/r
           fN r l a (n-o*r) (i-o*l) (x-o*a) 
  (fN n 1L 0L x 0L 1L)%n

let invertDeal n t = 
    match t with
    | NewStack -> NewStack
    | Cut cut -> Cut ((2L*n - int64 cut)%n)
    | Increment inc -> Increment (ModularMultiplicativeInverse n inc)

type affineMap = {a:int64; b:int64}

let compose (n:int64) (m2:affineMap) (m1:affineMap) =
    let newA = (int64(((bigint m1.a) * (bigint m2.a))%(bigint n)) + n)%n
    let newB = ((int64(((bigint m1.a) * (bigint m2.b))%(bigint n)) + m1.b)%n + n)%n
    {a = newA; b = newB}

let convertToAffineMap d= 
    match d with
    | NewStack -> {a = -1L; b = -1L}
    | Cut cut  -> {a = 1L; b = -cut}
    | Increment inc -> { a = inc; b = 0L}

let apply n m (idx:int64) = 
    (((int64 ((bigint m.a) * (bigint idx))%n + n)%n + m.b)%n + n)%n

let part2 = 
    let cardsize = 119315717514047L
    let iterations = 101741582076661L

    let transform = 
        input
        |> Array.map parse
        |> Array.rev
        |> Array.map (invertDeal cardsize)
        |> Array.map (convertToAffineMap )
        |> Array.reduce (compose cardsize)

    let rec tobinary n =
        if n = 1L then 
            "1" 
        else
            (n%2L).ToString() + tobinary (n/2L)

    let k = 
        (tobinary iterations).ToCharArray()
        |> Array.tail
        |> Array.scan(fun (x,y) t ->
            let z = compose cardsize y y
            if t = '0' then ({a=1L; b=0L},z) else (z,z)
            ) (transform,transform)
        |> Array.map fst
        |> Array.reduce (compose cardsize)

    apply cardsize k 2020L