let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day21.txt") 

open System.Text.RegularExpressions

type operation = 
    | SwapPosition of int*int
    | SwapLetter of (char*char)
    | RotateRight of int
    | RotateLeft of int
    | RotatePosition of char
    | Reverse of (int*int)
    | Move of (int*int)

let digitPair (line:string) = 
    let m = Regex.Match(line, @"(\d+).+(\d+)")
    let digit1 = m.Groups.[1].Value |> int
    let digit2 = m.Groups.[2].Value |> int
    digit1,digit2

let singleDigit (line:string) = 
    let m = Regex.Match(line, @"(\d+)")
    let digit = m.Groups.[1].Value |> int
    digit

let singleLetter (line:string) =
    let m = Regex.Match(line, @"letter (\w)")
    let letter = m.Groups.[1].Value |> Seq.head
    letter

let letterPair (line:string) =
    let m = Regex.Match(line, @"letter (\w) .+ letter (\w)")
    let letter1 = m.Groups.[1].Value |> Seq.head
    let letter2 = m.Groups.[2].Value |> Seq.head
    letter1, letter2

let parse (line:string) = 
    if line.StartsWith "move position" then
        line |> digitPair |> Move
    else if line.StartsWith "rotate right" then
        line |> singleDigit |> RotateRight
    else if line.StartsWith "rotate left" then
        line |> singleDigit |> RotateLeft
    else if line.StartsWith "rotate based" then
        line |> singleLetter |> RotatePosition
    else if line.StartsWith "swap position" then
        line |> digitPair |> SwapPosition
    else if line.StartsWith "swap letter" then
        line |> letterPair |> SwapLetter 
    else if line.StartsWith "reverse" then
        line |> digitPair |> Reverse
    else
        failwith (sprintf "unable to parse: %s" line)

type state =
    {
        IdxLetter : Map<int,char>
        LetterIdx : Map<char,int>
    }

let reverse (m:Map<'a,'b>) :Map<'b,'a> = m |> Map.fold(fun s k t -> s.Add(t,k)) Map.empty
    
let createState (str:string) = 
    let idxletter = 
        str.ToCharArray() 
        |> Seq.indexed
        |> Map
    {IdxLetter = idxletter; LetterIdx = reverse idxletter}

let rec operate state operation= 
    match operation with
    | SwapPosition(position1,position2) -> 
        let letter1 = state.IdxLetter.[position1]
        let letter2 = state.IdxLetter.[position2]
        {IdxLetter = state.IdxLetter.Add(position1, letter2).Add(position2,letter1); LetterIdx = state.LetterIdx.Add(letter1,position2).Add(letter2,position1)}
    | SwapLetter(letter1,letter2) -> 
        operate state (SwapPosition(state.LetterIdx.[letter1], state.LetterIdx.[letter2])) 
    | RotateRight steps -> 
        let n = state.IdxLetter.Count
        let letteridx  = state.LetterIdx |> Map.map(fun k v -> (v+steps)%n)
        {IdxLetter = letteridx |> reverse; LetterIdx = letteridx}
    | RotateLeft steps -> 
        let n = state.IdxLetter.Count
        operate state (RotateRight (n-steps)) 
    | RotatePosition letter -> 
        let idx = state.LetterIdx.[letter]
        let steps = if idx < 4 then idx + 1 else idx + 2
        operate state (RotateRight steps) 
    | Move (position1, position2) -> 
        let swaps = if position1 < position2 then [position1..(position2-1)] else (List.rev [position2..(position1-1)])
        swaps 
        |> List.map(fun idx -> SwapPosition(idx,idx+1))
        |> List.fold(fun s swap -> operate s swap) state
    | Reverse (position1,position2) -> 
        [0..abs((position1-position2)/2)]
        |> List.map(fun offset -> if position1 < position2 then SwapPosition (position1+offset, position2 - offset) else SwapPosition (position1-offset, position2 + offset))
        |> List.fold(fun s swap -> operate s swap) state

let scramble pw operations = 
    operations
    |> Seq.fold operate (createState pw)
    |> fun s -> s.IdxLetter
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map string
    |> String.concat ""

let instructions = input |> Array.map parse

let part1 = scramble "abcdefgh" instructions

let part2 = 
    let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))
    
    let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))
    
    let allPasswords = 
        permutations ("abcdefgh".ToCharArray() |> Array.toList)
        |> Seq.map (List.map string)
        |> Seq.map (String.concat "")

    allPasswords
    |> Seq.filter(fun pw -> scramble pw instructions = "fbgdceah")
    |> Seq.exactlyOne