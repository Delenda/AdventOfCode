let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +   @"\Input\Day25.txt") 
let card_pk = input.[0] |> uint64
let door_pk = input.[1] |> uint64
let modulus = 20201227UL

let loop_size pk = 
    Seq.initInfinite id
    |> Seq.scan(fun s _ -> (7UL*s)%modulus) 1UL
    |> Seq.takeWhile(fun s -> s <> pk)
    |> Seq.length
    |> uint64

let card_ls = loop_size card_pk

let encrypt subject (loopsize:uint64) = 
    Seq.initInfinite id
    |> Seq.scan(fun s _ -> (subject*s)%modulus) 1UL
    |> Seq.take (int loopsize + 1)
    |> Seq.last

let part1 = encrypt door_pk card_ls