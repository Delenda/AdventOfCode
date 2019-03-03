let test actual expected = 
    if actual  = expected then "ok"
    else failwith "Fail"
let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day01.txt")
let increments = input |> Array.map int
type state = {seen : Set<int>; result : int option}

let findRepeatedNumber (incs: int array) = 
    let examineNumber s t = 
        let newSeen = s.seen.Add t
        if s.seen.Contains t then
            {s with result = Some t}
        else
            {s with seen = newSeen}
    Seq.initInfinite(fun i -> incs.[i%(incs.Length)])
    |> Seq.scan (+) 0
    |> Seq.scan examineNumber {seen = Set.empty; result = None}
    |> Seq.filter(fun s -> s.result.IsSome)
    |> Seq.head
    |> fun s -> s.result.Value

let part2Test1 = test 0 (findRepeatedNumber [|1; -1|])
let part2Test2 = test 10 (findRepeatedNumber [|+3; +3; +4; -2; -4|])
let part2Test3 = test 5 (findRepeatedNumber [|-6; +3; +8; +5; -6|])
let part2Test4 = test 14 (findRepeatedNumber [|+7; +7; -2; -7; -4|])

let part1 = increments |> Array.sum
let part2 = increments |> findRepeatedNumber