let part1testinput = 
    [|
        "abcdef"
        "bababc"
        "abbcde"
        "abcccd"
        "aabcdd"
        "abcdee"
        "ababab"
    |]
let part2testinput =
    [|
        "abcde"
        "fghij"
        "klmno"
        "pqrst"
        "fguij"
        "axcye"
        "wvxyz"
    |] 

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day02.txt")

let hasRepeatedLetters n (s:string)=
    s.ToCharArray() |> Array.groupBy id |> Array.filter(fun x -> (snd x) |> Array.length = n) |> Array.isEmpty |> not

let checksum codes = 
    let twoLetters = codes |> Array.filter (hasRepeatedLetters 2) |> Array.length
    let threeLetters = codes |> Array.filter (hasRepeatedLetters 3) |> Array.length
    twoLetters * threeLetters

checksum part1testinput

let compareCodes ((a,b) : (string*string)) =
    let aLetters=  a.ToCharArray()
    let bLetters = b.ToCharArray()
    let letterCount = bLetters.Length
    let l = [0..letterCount-1] |> List.filter(fun i -> aLetters.[i] <> b.[i]) 
    if l.Length = 1 then
        let idx = l |> List.exactlyOne
        [0..letterCount-1] 
        |> List.filter(fun i -> i <> idx) 
        |> List.map(fun i -> aLetters.[i] |> string)
        |> Array.ofList 
        |> String.concat "" 
        |> Some
    else
        None

let findCode codes = 
    codes
    |> Array.collect(fun x -> codes |> Array.map(fun y -> (x,y) ))
    |> Array.filter(fun x -> fst x > snd x)
    |> Array.choose compareCodes
    |> Array.exactlyOne

findCode part2testinput

let part1 = 
    checksum input

let part2 = 
    findCode input