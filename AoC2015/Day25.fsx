let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day25.txt")

let row, column = 
    let m = System.Text.RegularExpressions.Regex.Match(input, @"row (\d+), column (\d+).")
    m.Groups.[1].Value |> int,
    m.Groups.[2].Value |> int

let number r c = 
    let n = r + c  
    (n-1)*(n-2)/2 + c

let part1 = 
    Seq.initInfinite id
    |> Seq.scan(fun s _ -> (s*252533UL)%33554393UL) 20151125UL
    |> Seq.take (number row column)
    |> Seq.last