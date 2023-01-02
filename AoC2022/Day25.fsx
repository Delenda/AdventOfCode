let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day25.txt") 

let convertSnafuDigit = function
    | '0' -> 0L
    | '1' -> 1L
    | '2' -> 2L
    | '-' -> -1L
    | '=' -> -2L
    | _ -> failwith "unexpected digit"

let convertDigit = function
    | 0L -> '0'
    | 1L -> '1'
    | 2L -> '2'
    | -1L -> '-'
    | -2L -> '='
    | _ -> failwith "unexpected digit"
    
let snafu2int (snafu:string) =
    snafu.ToCharArray()
    |> Array.map convertSnafuDigit
    |> Array.fold(fun s t -> 5L*s + t) 0L

let rec int2snafu (i:int64) (snafu:int64 list) : int64 list=
    if i = 0L then snafu else
    let d = i%5L
    if d < 3L then int2snafu (i/5L) (d::snafu) else
    int2snafu (i/5L+1L) ((d-5L)::snafu)

let part1 = 
    let sum  = input |> Array.map snafu2int |> Seq.sum
    (int2snafu sum []) |> Seq.map convertDigit |> Seq.map string |>  String.concat ""
