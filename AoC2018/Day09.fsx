let num_players = 419UL
let rounds = 71052
type dlist = 
    {left : uint64 list; right : uint64 list}
    member this.appendLeft a = 
        {left = a::this.left; right = this.right}
    member this.stepLeft = 
        if this.right.IsEmpty then
            if this.left.IsEmpty then
                {left = []; right = []}
            else
                {left = []; right = this.left |> List.rev}.stepLeft
        else
            {left = this.right.Head::this.left; right = this.right.Tail}
    member this.stepRight =
        if this.left.IsEmpty then
            if this.right.IsEmpty then
                {left = []; right = []}
            else
                {left = this.right |> List.rev; right = []}.stepRight
        else
            {left = this.left.Tail; right = this.left.Head :: this.right}
    member this.moveleft7 =
        this.stepLeft.stepLeft.stepLeft.stepLeft.stepLeft.stepLeft.stepLeft
    member this.moveright2 = 
        this.stepRight.stepRight
    member this.popLeft = 
        this.left.Head, {left = this.left.Tail ; right = this.right}

type dstate = {board : dlist; score : Set<uint64*uint64>; round:uint64}
let dini = {board = {left = [0UL]; right = []}; score = Set.empty; round = 1UL}

let play s=
    if s.round%23UL = 0UL then
        let n, newBoard = s.board.moveleft7.popLeft
        let score = n + s.round
        let q = {board = newBoard; score = s.score.Add (s.round, score); round = s.round + 1UL}
        Some(q,q)
    else
        let newBoard = s.board.moveright2.appendLeft s.round
        let q = {board = newBoard; score = s.score; round = s.round + 1UL}
        Some (q,q)

let findWinnerD np r =
    let res = 
        Seq.unfold play dini 
        |> Seq.take r
        |> Seq.last
    res.score 
        |> Set.filter(fun s -> snd s <> 0UL)
        |> Seq.groupBy(fun s -> (fst s)%np)
        |> Seq.maxBy(fun s -> (snd s) |> Seq.sumBy snd)
        |> snd
        |> Seq.sumBy snd

//findWinnerD 9UL 25
//findWinnerD 13UL 7999
//findWinnerD 17UL 1104
//findWinnerD 21UL 6111
//findWinnerD 30UL 5807

let part1 = 
    findWinnerD num_players rounds
let part2 = 
    findWinnerD num_players (rounds*100)
//type state = {board : uint64 List; score : Set<uint64*uint64>; round:uint64; bordLength:uint64}
//let initialState = {board = [0UL]; score = Set.empty; round = 1UL; bordLength = 1UL}

//let placeMarble s m = 
//        if s.round % 100000UL = 0UL then (System.Console.WriteLine s.round) else ()
//        let newBoard = 
//            if s.round = 1UL then
//                [1UL;0UL]
//            else
//                let x,y = s.board |> List.splitAt 2
//                s.round::y@x
//        let score,finalBoard, newBoardLength = 
//            if s.round%23UL = 0UL then
//                let c,d =List.splitAt 2 newBoard
//                let a,b = List.splitAt ((int s.bordLength) - 10) d
//                s.round + b.Head, b.Tail@c.Tail@a , s.bordLength - 1UL
//            else    
//                0UL, newBoard, s.bordLength + 1UL
//        let newScore = 
//            if score <> 0UL then
//                s.score |> Set.add (s.round, score)
//            else
//                s.score
//        {board = finalBoard; score = newScore; round = s.round + 1UL; bordLength = newBoardLength}

//let findWinner np r =
//    let res = [1UL..r] |> List.fold placeMarble initialState
//    res.score 
//        |> Set.filter(fun s -> snd s <> 0UL)
//        |> Seq.groupBy(fun s -> (fst s)%np)
//        |> Seq.maxBy(fun s -> (snd s) |> Seq.sumBy snd)
//        |> snd
//        |> Seq.sumBy snd

////findWinner num_players (rounds)

//let rec flet (a: uint64 list) (b:uint64 list) =
//    if a |> List.isEmpty then
//        []
//    else a.Head::b.Head::(flet a.Tail (b.Tail))

//type state23 = {board : uint64 list; m : uint64}
//let step23 s=
//    if s.m = 1UL then
//        Some(32UL, {s with m = 2UL})
//    else if s.m = 2UL then 
//        Some(63UL, {s with m = 3UL})
//    else
//        let vals = [(s.m-1UL)*23UL+1UL..s.m*23UL-1UL]
//        let x,y = List.splitAt 23 s.board
//        let kn = flet x.Tail vals
//        let a, b = List.splitAt 36 kn
//        let newb = b.Tail@y@(x.Head::a)
//        let newS = b.Head + (s.m*23UL)
//        Some(newS, {board = newb; m = s.m + 1UL})
//let ini23 = {board = [42UL; 4UL; 43UL; 18UL; 44UL; 19UL; 45UL; 2UL; 24UL; 20UL; 25UL; 10UL;
//      26UL; 21UL; 27UL; 5UL; 28UL; 22UL; 29UL; 11UL; 30UL; 1UL; 31UL; 12UL;
//      32UL; 6UL; 33UL; 13UL; 34UL; 3UL; 35UL; 14UL; 36UL; 7UL; 37UL; 15UL;
//      38UL; 0UL; 39UL; 16UL; 40UL; 8UL; 41UL]; m = 1UL}

//let findWinner2 np r =
//    Seq.unfold step23 ini23 
//    |> Seq.take (r/23)
//    |> Seq.mapi(fun i x -> (((i + 1)*23)%np, x)  )
//    |> Seq.groupBy fst
//    |> Seq.maxBy(fun x -> (snd x) |> Seq.sumBy snd)
//    |> snd
//    |> Seq.sumBy snd

//findWinner2 9 25
//findWinner2 13 7999
//findWinner2 17 1104
//findWinner2 21 6111
//findWinner2 30 5807
//findWinner2 419 71052

//71052/23
//Seq.unfold step23 ini23  |> Seq.take 308000 |> Seq.last

//type dlist = 
//    {left : uint64 list; right : uint64 list}
//    member this.appendLeft a = 
//        {left = a::this.left; right = this.right}
//    member this.appendRight a = 
//        {left = this.left; right = a::this.right}
//    member this.stepLeft = 
//        if this.right.IsEmpty then
//            if this.left.IsEmpty then
//                {left = []; right = []}
//            else
//                {left = []; right = this.left |> List.rev}.stepLeft
//        else
//            {left = this.right.Head::this.left; right = this.right.Tail}
//    member this.stepRight =
//        if this.left.IsEmpty then
//            if this.right.IsEmpty then
//                {left = []; right = []}
//            else
//                {left = this.right |> List.rev; right = []}.stepRight
//        else
//            {left = this.left.Tail; right = this.left.Head :: this.right}
//    member this.moveleft7 =
//        this.stepLeft.stepLeft.stepLeft.stepLeft.stepLeft.stepLeft.stepLeft
//    member this.moveright2 = 
//        this.stepRight.stepRight
//    member this.popLeft = 
//        this.left.Head, {left = this.left.Tail ; right = this.right}
//    member this.popRight = 
//        this.right.Head, {left = this.left; right = this.right.Tail}

// type dstate = {board : dlist; score : Set<uint64*uint64>; round:uint64}
// let dini = {board = {left = [0UL]; right = []}; score = Set.empty; round = 1UL}

// let play s=
//    if s.round%23UL = 0UL then
//        let n, newBoard = s.board.moveleft7.popLeft
//        let score = n + s.round
//        let q = {board = newBoard; score = s.score.Add (s.round, score); round = s.round + 1UL}
//        Some(q,q)
//    else
//        let newBoard = s.board.moveright2.appendLeft s.round
//        let q = {board = newBoard; score = s.score; round = s.round + 1UL}
//        Some (q,q)
 




//findWinnerD 9UL 25
//findWinnerD 13UL 7999
//findWinnerD 17UL 1104
//findWinnerD 21UL 6111
//findWinnerD 30UL 5807
//findWinnerD 419UL 7105200

System.UInt32.MaxValue