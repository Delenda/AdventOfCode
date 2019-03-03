let input = "hxbxwxba"

let getCarry pw = 
    pw |> Array.scan (fun s t -> max ((t + s) - 25) 0 ) 1 |> Array.tail

let inc pw =
    let carry = getCarry pw
    pw |> Array.mapi(fun i x -> match i with | 0 -> x + 1 | _ -> x + carry.[i-1]) |> Array.map(fun x -> x%26)

let step pw = 
    let criteria1 = pw |> Array.windowed 3 |> Array.filter(fun s -> s.[0] = s.[1] + 1 && s.[1] = s.[2] + 1 ) |> Array.length > 0
    let criteria2 = pw |> Array.filter(fun c -> c = (int 'i' - int 'a') || c = (int 'o' - int 'a') || c = (int 'l' - int 'a')) |> Array.length = 0
    let criteria3 = [0..(pw.Length-4)] |> List.exists(fun i -> pw.[i] = pw.[i+1] && [i+2..(pw.Length-2)] |> List.exists(fun j -> pw.[j] = pw.[j+1] && pw.[i] <> pw.[j]))
    match (criteria1 && criteria2 && criteria3) with
    | true -> None
    | false -> 
        let newPw = inc pw
        Some( newPw, newPw)

let strToPw (str:string) = 
    str.ToCharArray() |> Array.map(fun c -> int c - (int 'a')) |> Array.rev

let pwToStr pw = 
    pw
        |> Seq.map(fun i -> i + int 'a') 
        |> Seq.map (char >> string) 
        |> Seq.toArray 
        |> Array.rev 
        |> String.concat ""

let question1 (str : string)= 
    let pw = strToPw str
    Seq.unfold step pw |> Seq.last |> pwToStr

let question2 str = 
    let pw = question1 str
    let pw1 = pw |> strToPw |> inc |> pwToStr
    question1 pw1

question1 input
question2 input