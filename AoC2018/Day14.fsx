let input = 327901

type state = {elf1 : int; elf2:int; length:int}

let Scoreboard size take length= 
    let sb = Array.replicate size 0
    sb.[0]<-3
    sb.[1]<-7
    let unfolder (s:state) = 
        let sum = sb.[s.elf1] + sb.[s.elf2]
        let newL  =
            if sum > 9 then
                sb.[s.length]<- (sum/10) 
                sb.[s.length + 1]<- (sum%10)
                s.length + 2
            else
                sb.[s.length]<-sum
                s.length + 1
        let newElf1 = (s.elf1 + sb.[s.elf1] + 1)%(newL)
        let newElf2 = 
            let a = (s.elf2 + sb.[s.elf2] + 1)%(newL)
            if a = newElf1 then
                (a + 1)%newL
            else
                a
        let q = {elf1 = newElf1; elf2 = newElf2; length = newL}
        Some(q,q)
    let rv = 
        Seq.unfold unfolder {elf1 = 0; elf2 = 1; length = 2}
        |> Seq.filter(fun s -> s.length >= length)
        |> Seq.take take
        |> Seq.last
    sb,rv

let getNext10 n =
    let sb, rv = Scoreboard (n+20) 1 (n+10)
    sb 
    |> Array.skip n 
    |> Array.take 10 
    |> Array.map string 
    |> String.concat "" 

let findSeq (n:string) =
    let sb, rv = Scoreboard 28000000 21000000 -1

    let seeker (sq: int array) = 
        Seq.initInfinite id
        |> Seq.filter
            (fun i -> 
                sq 
                |> Array.mapi(fun j b -> sb.[i+j+1] = b) 
                |> Array.reduce(&&))
        |> Seq.head
        |> (+) 1

    n.ToCharArray()
    |> Array.map (string >> int)
    |> seeker

let part1 = 
    getNext10 input

let part2 =
    findSeq (string input)
