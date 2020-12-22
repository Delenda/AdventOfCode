let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day22.txt")
open System.Collections.Generic

let player1, player2 = 
    let parts = input.Split([|"\r\n\r\n"|], unbox 0)
    parts.[0].Split([|"\r\n"|], unbox 0) |> Array.tail |> Array.map int |> Array.toList,    parts.[1].Split([|"\r\n"|], unbox 0) |> Array.tail |> Array.map int  |> Array.toList

let runGame gameAlgorithm initialHand1 initialHand2= 
    Seq.unfold(fun (hand1,hand2) -> 
        if List.isEmpty hand1 || List.isEmpty hand2 then None
        else
        let h1,h2 = gameAlgorithm hand1 hand2
        let newHand1 = hand1.Tail @ h1
        let newHand2 = hand2.Tail @ h2
        let newState = newHand1, newHand2
        Some(newState, newState)
        ) (initialHand1, initialHand2)
    |> Seq.last

let Combat hand1 hand2 = 
    let h1 = List.head hand1
    let h2 = List.head hand2
    if h1 > h2 then 
        [h1;h2], []
    else
        [], [h2;h1]

type cache = {Seen1 : HashSet<int list>;Seen2 : HashSet<int list>}
let rec RecursiveCombat (seen:cache) hand1 hand2 = 
    let h1 = List.head hand1
    let h2 = List.head hand2
    if seen.Seen1.Contains hand1 || seen.Seen2.Contains hand2 then [h1;h2],[] else
    seen.Seen1.Add hand1 |> ignore
    seen.Seen2.Add hand2 |> ignore
    let player1Wins = 
        if (hand1.Tail.Length < h1) || (hand2.Tail.Length < h2) then h1 > h2 else
        determineWinner hand1 hand2
    if player1Wins then 
        [h1;h2], []
    else
        [], [h2;h1]
and determineWinner hand1 hand2 = 
    let newSeen = {Seen1 = new HashSet<int list>(); Seen2 = new HashSet<int list>()}
    let h1, h2 = runGame (RecursiveCombat newSeen) (hand1.Tail |> List.take hand1.Head) (hand2.Tail |> List.take hand2.Head)
    h2 |> List.isEmpty

let score (res1, res2) = 
    (res1, res2)
    |> fun (a,b) -> a@b
    |> List.rev 
    |> List.indexed 
    |> List.sumBy(fun (i,c) -> (i+1)*c)

let part1 = 
    runGame Combat player1 player2 |> score    

let part2 = 
    let seen = {Seen1 = new HashSet<int list>(); Seen2 = new HashSet<int list>()}
    runGame (RecursiveCombat seen) player1 player2 |> score