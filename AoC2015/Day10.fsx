let input = "3113322113"

let lookAndSee (str : string) _ =
    let division = [1..(str.Length-1)] |> List.filter(fun i -> str.[i] <> str.[i-1])
    let a = List.zip ([0] @ division) (division @ [str.Length])
    a |> List.map(fun (x,y) -> (y-x).ToString() + (string str.[x])) |> String.concat ""

let question n =
    [1..n] |> List.fold lookAndSee input |> Seq.length

let question1 = question 40
let question2 = question 50