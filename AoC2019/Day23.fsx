let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ +   @"\Input\Day23.txt") 

#load "IntCode.fsx"

let initialNics : (IntCode.programState array) = 
    Array.replicate 50 (IntCode.createMemory(input).Add(-1L,0L))
    |> Array.mapi(fun i ic -> 0L, ic, [], [int64 i])

let networkStep stopcondition ((nics, nat, natYprev, natYcurrent) : IntCode.programState array * int64 list * int64 * int64) =
    let progressedNics : IntCode.programState array= 
        nics
        |> Array.map( Seq.unfold IntCode.programStep >> Seq.last)
    let packets = 
        progressedNics
        |> Array.choose(fun (_,_,output,_) -> 
            if output.IsEmpty then 
                None 
            else 
                output
                |> List.rev
                |> List.chunkBySize 3
                |> List.map(fun l -> l.Head, l.Tail)
                |> List.toArray
                |> Some)
        |> Array.collect id
        |> Array.groupBy fst
        |> Array.map(fun (id, l) -> id, l |> Array.toList |> List.collect snd)
        |> Map
    let nicsWithInput : IntCode.programState array= 
        progressedNics
        |> Array.mapi(fun i (position, memory, _, input) -> 
            if packets.ContainsKey(int64 i) then 
                (position, memory, [], input@packets.[int64 i]) 
            else if input.IsEmpty then
                (position, memory, [], [-1L])
            else
                (position, memory, [], input))
    let newNat = 
        if packets.ContainsKey 255L then 
            packets.[255L] 
            |> List.rev
            |> List.take 2
            |> List.rev
        else    
            nat
    let newNics, newNatYprev, newNatYcurrent = 
        if nicsWithInput = nics then
            nicsWithInput |> Array.mapi(fun i (p,m,_,inp) -> if i = 0 then (p,m,[],newNat) else (p,m,[],inp)), natYcurrent, newNat |> List.tail |> List.head 
        else
            nicsWithInput, natYprev, natYcurrent
    if stopcondition newNatYcurrent newNatYprev then None else
    Some(newNatYcurrent, (newNics, newNat, newNatYprev, newNatYcurrent))

let repeatedNatYValue a b = 
    a = b

let packetSentToNat a b = 
    b > 0L

let part1 = 
    Seq.unfold (networkStep packetSentToNat)  (initialNics,[],-1L,0L) |> Seq.last
let part2 = 
    Seq.unfold (networkStep repeatedNatYValue)  (initialNics,[],-1L,0L) |> Seq.last