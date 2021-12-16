let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day16.txt") 

let convertToBinary (str:string) = 
    str.ToCharArray()
    |> Seq.map(fun x -> System.Convert.ToInt32(string x, 16) )
    |> Seq.collect(fun x -> System.Convert.ToString(x,2).PadLeft(4,'0').ToCharArray())
    |> Seq.toList

type packet = 
    { Version : int64
      ID : int64
      SubPackets : packet list
      Litteral : int64 option}

let parseInt (stream:char seq) = 
    stream |> Seq.map string |> String.concat "" |> fun s -> System.Convert.ToInt64(s,2)

let parseHeader stream = 
    let version = stream |> List.take 3 |> parseInt
    let typeid  = stream |> List.skip 3 |> List.take 3 |> parseInt
    version,typeid, stream |> List.skip 6

let parseLitteral stream = 
    let chunkCount = stream |> List.chunkBySize 5 |> List.takeWhile(fun s -> s |> List.head = '1') |> List.length |> (+) 1
    let integer = 
        stream
        |> List.chunkBySize 5
        |> List.take chunkCount
        |> List.collect List.tail
        |> parseInt
    integer, stream |> List.skip (5*chunkCount)

let rec parse stream : packet list = 
    if stream |> Seq.forall((=) '0') then [] else
    let packet, rest = parsePacket stream
    packet :: (parse rest)
and  parsePacket stream = 
    let version,typeid, rest = parseHeader stream
    match typeid with
    | 4L -> let integer, finalRest =  parseLitteral rest
            {Version = version ; ID = typeid; SubPackets = []; Litteral = Some integer}, finalRest
    | _ ->  let packets, finalRest = parseOperator rest
            {Version = version ; ID = typeid; SubPackets = packets; Litteral = None}, finalRest
and parseOperator stream = 
    match stream |> List.head with
    | '0' -> 
        let length = stream |> List.skip 1 |> List.take 15 |> parseInt |> int
        parse (stream |> List.skip 16 |> List.take length), stream |> List.skip(16+length)
    | '1' -> 
        let packetCount = stream |> List.skip 1 |> List.take 11 |> parseInt
        let p,r = 
            [1L..packetCount] |> List.fold(fun (packets, rest) _-> 
                let packet, newRest = parsePacket rest
                packet::packets, newRest) ([], stream |> List.skip 12)
        p |> List.rev, r
    | _ -> "unexpected token" |> failwith

let packet (str:string) = input |> convertToBinary |> parse |> List.head

let rec totalVersion (packet : packet) = 
    packet.Version + (packet.SubPackets |> List.sumBy totalVersion)
    
let rec evaluate (packet:packet) = 
    match packet.ID with
    | 0L -> packet.SubPackets |> List.sumBy evaluate 
    | 1L -> packet.SubPackets |> List.map evaluate |> List.reduce (*)
    | 2L -> packet.SubPackets |> List.map evaluate |> List.min
    | 3L -> packet.SubPackets |> List.map evaluate |> List.max
    | 4L -> packet.Litteral.Value
    | 5L -> packet.SubPackets |> List.map evaluate |> fun l -> if l.Head > l.Tail.Head then 1L else 0L
    | 6L -> packet.SubPackets |> List.map evaluate |> fun l -> if l.Head < l.Tail.Head then 1L else 0L
    | 7L -> packet.SubPackets |> List.map evaluate |> fun l -> if l.Head = l.Tail.Head then 1L else 0L
    | _ -> "unexpected typeid" |> failwith
    
let part1 = packet input |> totalVersion
let part2 = packet input |> evaluate