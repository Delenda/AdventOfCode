let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day04.txt")
let passphrases = input |> Array.map(fun s -> s.Split(' '))
let wordcount = passphrases |> Array.map(fun a -> a.Length)
let disinctWordcount = passphrases |> Array.map( Array.sort >> Array.distinct >> Array.length)
let sortPhrase (phrase : string) = phrase.ToCharArray() |> Array.sort |> Array.map string |> Array.reduce (+)
let distinctAnagrams = passphrases |> Array.map(Array.map sortPhrase >> Array.distinct >> Array.length)

let question1 = Array.zip wordcount disinctWordcount |> Array.filter(fun x -> fst x = snd x) |> Array.length
let question2 = Array.zip wordcount distinctAnagrams |> Array.filter(fun x -> fst x = snd x) |> Array.length