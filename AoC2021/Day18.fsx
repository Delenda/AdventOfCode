let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day18.txt") 

let snail2tokens (snail:string) =   
    snail.ToCharArray() 
    |> Array.toList 
    |> List.filter(fun c -> c <> ',') 
    |> List.map(fun c -> 
        match c with 
        | '[' -> -1 
        | ']' -> -2 
        | _ -> c |> string |> int)

let snails = input |> Array.map snail2tokens |> Array.toList

let explode (snail: int list) = 
    snail  
    |> List.fold(fun (tokens,count,carry) token -> 
        let newToken, newCarry = 
            if token < 0 then token,carry else token + carry,0
        if count = 5 then 
            let lastDigit = tokens |> List.tryFindIndex(fun t -> t >= 0)
            let newTokens = 
                if lastDigit.IsSome then 
                    let a,b = tokens |> List.splitAt lastDigit.Value
                    0::a.Tail@(b.Head + newToken)::b.Tail
                else 
                    0::tokens.Tail
            newTokens, 6, 0
        else if count = 6 then
            tokens, 7, token
        else if count = 7 then 
            tokens, 4, newCarry
        else
        let newCount = 
            if token = -1 then count + 1
            else if token = -2 then count - 1
            else count
        newToken::tokens, newCount, newCarry
        ) ([],0,0)
    |> fun (x,_,_) -> x 
    |> List.rev

let split(snail:int list) =
    let splitpoint = snail |> List.tryFindIndex(fun t -> t > 9)
    if splitpoint.IsNone then snail else 
        let a, b = snail |> List.splitAt splitpoint.Value
        a@[-1;b.Head/2;b.Head - b.Head/2; -2]@b.Tail

let addSnails (snail1:int list) (snail2:int list) = 
    let totalSnail = -1::snail1@snail2@[-2]
    let rec reduce snail = 
        let newSnail = snail |> explode |> split
        if snail = newSnail then snail
        else
        reduce newSnail
    reduce totalSnail

type snailNumber = 
    | Leaf of int
    | Branch of Left : snailNumber * Right : snailNumber

let rec ParseSnail (snail: int list) =
    if snail.Head >= 0 then Leaf snail.Head, snail.Tail
    else
    let left, rest = ParseSnail snail.Tail
    let right, rest2 = ParseSnail rest
    Branch(left,right), rest2.Tail

let rec Magnitude = function
    | Leaf regularNumber -> regularNumber
    | Branch (left, right) -> 3 * (Magnitude left) + 2 * (Magnitude right)
    
let evaluate = ParseSnail >> fst >> Magnitude

let rec printSnail = function
    | Leaf regularNumber -> sprintf "%d" regularNumber
    | Branch (left,right) -> sprintf "[%s,%s]" (printSnail left) (printSnail right)

let part1 = 
    snails
    |> List.reduce addSnails
    |> evaluate

let part2  =
    seq{for s1 in snails do
        for s2 in snails do
        if s1 <> s2 then yield (s1,s2)}
    |> Seq.map(fun (s1,s2) -> addSnails s1 s2)
    |> Seq.map evaluate
    |> Seq.max