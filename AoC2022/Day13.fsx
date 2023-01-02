let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day13.txt")
    
let isDigit = System.Char.IsDigit

type packet = 
   | Integer of int
   | PacketList of packet list

let parse (packet:string) = 
    let rec innerParse tokens=
        match tokens with
        | '['::tail           -> parseBracket tail []
        | ','::tail           -> innerParse tail
        | x::_ when isDigit x -> parseInt tokens 0 |> fun (number,rest) -> Integer number, rest
        | _ -> failwith "unexpected token"
    and parseBracket tokens packets = 
        if tokens.Head = ']' then 
            PacketList (List.rev packets), tokens.Tail 
        else
            let packet,rest = innerParse tokens
            parseBracket rest (packet::packets)
    and parseInt tokens number= 
        if tokens.Head |> isDigit |> not then 
            number, tokens
        else
            let d = tokens.Head |> string |> int
            parseInt tokens.Tail (10*number + d)
    
    packet
    |> Seq.toList
    |> innerParse 
    |> fst

let packetPairs = 
    input
    |> Array.chunkBySize 3
    |> Array.map(fun packetPair -> parse packetPair.[0], parse packetPair.[1])

let rec compare packet1 packet2 =
    if packet1 = packet2  then 0 else
    match packet1,packet2 with
    | Integer i1, Integer i2 -> i1 - i2
    | Integer _, PacketList _ -> compare (PacketList [packet1]) packet2
    | PacketList _, Integer _ -> compare packet1 (PacketList [packet2])
    | PacketList p1, PacketList p2 -> 
        if p1.IsEmpty then -1 else
        if p2.IsEmpty then 1 else
        let r = compare p1.Head p2.Head
        if r <> 0 then r else 
        compare (PacketList p1.Tail) (PacketList p2.Tail)

let part1 = 
    packetPairs 
    |> Seq.mapi(fun idx (p1,p2) -> idx+1, compare p1 p2)
    |> Seq.filter(fun x -> snd x < 0)
    |> Seq.sumBy fst

let part2 = 
    let divider1 = parse "[[2]]"
    let divider2 = parse "[[6]]"
    let indices = 
        packetPairs
        |> Seq.collect(fun (a,b) -> [a;b])
        |> Seq.append [divider1;divider2]
        |> Seq.sortWith compare
        |> Seq.mapi(fun idx packet -> packet, idx + 1)
        |> Map
    indices.[divider1] * indices.[divider2]
