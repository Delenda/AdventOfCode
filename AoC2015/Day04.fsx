let input = "iwrupvqb"

let md5 = System.Security.Cryptography.MD5.Create()

let question length = 
    let generateHash i =
        let str = input + i.ToString()    
        (i, md5.ComputeHash (str.ToCharArray() |> Array.map byte) |> Array.map(fun b -> b.ToString("x2")) |> String.concat "")
    let pattern = Array.replicate length "0" |> String.concat ""
    Seq.initInfinite generateHash |> Seq.filter(fun (x,y) -> y.StartsWith(pattern)) |> Seq.head |> fst

let question1 = question 5
let question2 = question 6