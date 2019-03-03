let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day16.txt")

type permutation =  | Spin of int
                    | Transposition of int * int
                    | Swap of char * char

let getPermutation (str : string) =
    match str.[0] with
    | 's' -> str.Substring(1) |> int |> fun s -> 16 - s |> Spin
    | 'x' -> 
             let t = (str.Substring(1).Split('/')) |> Array.map (string >> int)
             Transposition (t.[0],t.[1])
    | 'p' -> Swap( str.[1], str.[3])
    |  _ -> failwith ("Unexpected  ")

let perms = (input |> Array.head).Split(',') |> Array.map getPermutation

let init = [0..15] |> List.map(fun i -> (int 'a') + i |> char) |> Array.ofList

let rec applyPermutation (state : char array) p =
    match p with
    | Spin ( s) -> let a, b = state |> Array.splitAt s
                   Array.concat[|b;a|]
    | Transposition(x,y) -> let a = state.[x]
                            let b = state.[y]
                            applyPermutation state (Swap(a,b))
    | Swap (x, y) ->  state |> Array.map(fun c ->
                                            if c = x then y
                                            else if c = y then x
                                            else c)

let findorder permArray =
    let rec seek seen current =
        if Set.contains current seen then 0
        else 1 + seek (Set.add current seen) ( permArray |> Array.fold applyPermutation current)
    seek Set.empty init
    
let question1 = //"gkmndaholjbfcepi"
    perms |> Array.fold applyPermutation init |> fun s -> System.String.Join("",s)

let question2 = // "abihnfkojcmegldp"
    let order = findorder perms
    [1..(1000000000 % order)] |> Array.ofList |> Array.collect(fun x -> perms) |> Array.fold applyPermutation init |> fun s -> System.String.Join("",s)