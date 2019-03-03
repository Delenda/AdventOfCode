let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day15.txt")
let getSeed (str : string) = str.Split(' ') |> Array.last |> uint64
let seedA = input |> Array.filter(fun s -> s.Contains("A")) |> Array.map getSeed |> Array.head
let seedB = input |> Array.filter(fun s -> s.Contains("B")) |> Array.map getSeed |> Array.head

let ga = 16807UL
let gb = 48271UL
let m8 = 2147483647UL // = 2^31 - 1, 8th Mersenne prime

let judge n getnextA getnextB sa sb =
    let bitmask = 65535UL // = "00000000000000001111111111111111"
    let step (a,b, cnt)  i =
        let na = getnextA a
        let nb = getnextB b
        match na ^^^ nb &&& bitmask with
        | 0UL -> na, nb, cnt + 1
        | _   -> na, nb, cnt
    [1..n] |> List.fold step (sa, sb, 0) |> fun (x,y,z) -> z

let question1 =
    let getnextA a = a * ga % m8
    let getnextB b = b * gb % m8
    judge 40000000 getnextA getnextB 

let question2 = 
    let rec findNext wg md aa =
        let naa = aa * wg % m8
        match naa % md with
        | 0UL -> naa
        | _   -> findNext wg md naa
    let getnextA = findNext ga 4UL
    let getnextB = findNext gb 8UL
    judge 5000000 getnextA getnextB         

question1 seedA seedB
question2 seedA seedB