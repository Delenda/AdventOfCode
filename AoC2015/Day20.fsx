let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ +   @"\Input\Day20.txt") |> int64
let factors number = seq {
    for divisor in 1L .. (float >> sqrt >> int64) number do
    if number % divisor = 0L then
        yield divisor
        if number <> 1L  then yield number / divisor //special case condition: when number=1 then divisor=(number/divisor), so don't repeat it
}

let numberOfPresents1 n =
    let sum = n|> int64 |> factors |> Seq.distinct |> Seq.sumBy((*) 10L)
    (n,sum)

let numberOfPresents2 n=
    let nL = int64 n
    let number = nL |> factors |> Seq.filter(fun d -> nL/d < 51L) |> Seq.distinct |> Seq.sumBy ((*) 11L)
    (n,number)

let question seed  =
    Seq.initInfinite seed |> Seq.filter(fun (x,y) -> y > input) |> Seq.head |> fst

let question1 = question numberOfPresents1
let question2 = question numberOfPresents2
