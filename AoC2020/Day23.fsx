let input = "925176834"
open System.Collections.Generic

let cyclicLinkedList (cups:string) (count:int option)= 
    let  cll = new Dictionary<int,int>()
    let initial = cups.ToCharArray() |> Seq.map string |> Seq.map int |> Seq.toList 
    initial |> List.windowed 2 |> List.iter(fun x -> cll.Add(x.Head, x.Tail.Head))
    if count.IsSome then
        let initCount = initial.Length
        [(initCount + 1)..(count.Value - 1)] |> List.iter(fun i -> cll.Add(i, i + 1))
        cll.Add((initial |> List.rev |> List.head), initCount + 1)
    cll.Add(count |> Option.defaultValue(initial |> List.rev |> List.head), initial.Head)
    cll

let rec destination current pickup1 pickup2 pickup3 maxcup = 
    let dest = if current > 1 then current - 1 else maxcup
    if dest <> pickup1 && dest <> pickup2 && dest <> pickup3 then dest else destination dest pickup1 pickup2 pickup3 maxcup

let play n (cups:Dictionary<int,int>) initialCurrent = 
    let maxCup = cups.Count
    Seq.unfold(fun (nb, current) -> 
        if nb = n then None else
        let pickup1 = cups.[current]
        let pickup2 = cups.[pickup1]
        let pickup3 = cups.[pickup2]
        let newCurrent = cups.[pickup3]
        let dest = destination current pickup1 pickup2 pickup3 maxCup
        let tmp = cups.[dest]
        cups.[dest]<- pickup1
        cups.[pickup3]<- tmp
        cups.[current]<-newCurrent
        let newState = (nb + 1 , newCurrent)
        Some(newState, newState)
        ) (0, initialCurrent)
    |> Seq.iter ignore

let part1 = 
    let current = 9
    let turns = 100
    let totalCupCount = None
    let cups = cyclicLinkedList input totalCupCount
    play turns cups current
    Seq.unfold(fun s -> Some(cups.[s],cups.[s])) 1
    |> Seq.take 8
    |> Seq.map string
    |> String.concat ""

let part2 = 
    let current = 9
    let turns = 10000000
    let totalCupCount = 1000000 |> Some
    let cups = cyclicLinkedList input totalCupCount
    play turns cups current
    (uint64 cups.[1]) *(uint64 cups.[cups.[1]])