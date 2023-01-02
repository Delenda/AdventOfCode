let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day20.txt") 

let ex = [|"1";"2";"-3";"3";"-2";"0";"4"|]

let digits = 
    input
    |> Seq.mapi(fun i x -> int64 i, int64 x )
    |> Map

let digit0 = digits |> Map.filter(fun k v -> v = 0L) |> Map.toSeq |> Seq.map fst |> Seq.exactlyOne

let size = digits.Count |>int64

//let init = [0L .. size-1L] |> Seq.map(fun i -> int64 i,int64 i) |> Map

//let moveRight t moves (num2Pos:Map<int64,int64>) (pos2Num:Map<int64,int64>) =
//    Seq.init (int (moves%(size-1L))) id
//    |> Seq.fold(fun ((n2p,p2n):Map<int64,int64>*Map<int64,int64>) _ -> 
//        let pos = n2p.[t]
//        let newPos = (pos+1L)%size
//        let other = p2n.[newPos]
//        n2p.Add(t,newPos).Add(other,pos), p2n.Add(newPos,t).Add(pos,other)
//        ) (num2Pos, pos2Num)

//let moveLeft t moves (num2Pos:Map<int64,int64>) (pos2Num:Map<int64,int64>) =
//    Seq.init (int (moves%(size-1L))) id
//    |> Seq.fold(fun ((n2p,p2n):Map<int64,int64>*Map<int64,int64>) _ -> 
//        let pos = n2p.[t]
//        let newPos = (pos-1L+size)%size
//        let other = p2n.[newPos]
//        n2p.Add(t,newPos).Add(other,pos), p2n.Add(newPos,t).Add(pos,other)
//        ) (num2Pos, pos2Num)

//let mix n decryption = 
//    let n2p, p2n =
//        Seq.init n id
//        |> Seq.fold(fun ((_num2Pos,_pos2Num):Map<int64,int64>*Map<int64,int64>) round-> 
//            digits
//            |> Map.toSeq
//            |> Seq.map(fun (a,b) -> a, b*decryption)
//            |> Seq.fold(fun ((num2Pos,pos2Num):Map<int64,int64>*Map<int64,int64>) (t,move) -> 
//                //if t%100L = 0L then sprintf "Round %d t: %d" round t |> System.Console.WriteLine
//                if move > 0L then 
//                    moveRight t (move) num2Pos pos2Num
//                else if move < 0L then 
//                    moveLeft t (-move) num2Pos pos2Num
//                else
//                    num2Pos, pos2Num) (_num2Pos,_pos2Num)
//            ) (init,init)

//    let n0 = n2p.[digit0]
//    let n1000 = (n0 + 1000L)%size
//    let n2000 = (n0 + 2000L)%size
//    let n3000 = (n0 + 3000L)%size
//    (digits.[p2n.[n1000]] + digits.[p2n.[n2000]] + digits.[p2n.[n3000]])*decryption
        
//let sw1 = System.Diagnostics.Stopwatch.StartNew()
//let part1 = mix 1 1L
//let duration1 = sw1.Elapsed.TotalSeconds
//let sw2 = System.Diagnostics.Stopwatch.StartNew()
//let part2 = mix 10 811589153L
//let duration2 = sw2.Elapsed.TotalSeconds


let print (cycle:Map<int64,int64>)  =
    let k = 
        Seq.init (int size) id |> Seq.fold(fun (n) _ -> 
            let d = digits.[n] |> sprintf "%d, " |> System.Console.Write
            cycle.[n]) 0L
    System.Console.WriteLine""

let mix_v2 n decryption= 
    let forward = [0L..size-1L] |> Seq.map(fun a -> a, (a + 1L)%size) |> Map 
    let backward = forward |> Map.toSeq |> Seq.map(fun (a,b) -> b,a) |> Map
    
    let links, _ =
        Seq.init n id
        |> Seq.fold(fun (forw,back) _ -> 
            digits
            |> Map.toSeq
            |> Seq.map(fun (a,b) -> a, b*decryption)
            |> Seq.fold( fun ((f,b) : Map<int64,int64>*Map<int64,int64>) (t,move) -> 
                if t%1000L = 0L then System.Console.WriteLine t
                let next = f.[t]
                let previous = b.[t]
                let steps_right = 
                    if move > 0L then 
                        move % (size-1L)
                    else
                        (size-1L) - ((-move)%(size-1L))

                let modF = f.Add(previous,next)
                let modB = b.Add(next,previous)
                let dest_next, dest_prev = 
                    Seq.init (int steps_right) id
                    |> Seq.fold(fun (x,y) _ -> modF.[x],modF.[y]) (next,previous)
                let newForward = modF.Add(dest_prev, t).Add(t,dest_next)
                let newBackward = modB.Add(t,dest_prev).Add(dest_next,t)
                //print newForward
                newForward, newBackward) (forw,back)
             ) (forward, backward)
    
    let n1000 = Seq.init 1000 id |> Seq.fold (fun s _ -> links.[s]) digit0
    let n2000 = Seq.init 1000 id |> Seq.fold (fun s _ -> links.[s]) n1000
    let n3000 = Seq.init 1000 id |> Seq.fold (fun s _ -> links.[s]) n2000
    (digits.[n1000] + digits.[n2000] + digits.[n3000])*decryption

let part1 = mix_v2 1 1L
let part2 = mix_v2 10 811589153L