let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day07.txt")

let parseStep (s:string) =
    let a = s.Substring(5,1)
    let b = s.Substring(36,1)
    (a,b)

let steps = input |> Array.map parseStep

let rec sift (stps : (string*string) array) =
    if (stps |> Array.length = 1) then
        let x = stps |> Array.exactlyOne
        (fst x) + (snd x)
    else
        let a = stps |> Array.map fst |> Array.distinct
        let b = stps |> Array.map snd |> Array.distinct
        let h = a |> Array.filter(fun s -> b |> Array.contains s |> not) |> Array.distinct |> Array.sort |> Array.take 1 |> Array.exactlyOne
        let nstps = stps |> Array.filter(fun s ->  fst s <> h)
        h + (sift nstps)

sift steps

let testinput = 
    "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."

let tst = testinput.Split('\n') |> Array.map parseStep

sift tst

type state = { workers : (string*int) array; stps : (string*string) array; count : int; Done : string; Last : Set<string>}

let last = Set.difference (steps |> Array.map snd |> Set.ofArray) (steps |> Array.map fst |> Set.ofArray) 
let ini = {workers = Array.replicate 6 ("",0); stps = steps; count = 0; Done = ""; Last = last}

//let run h delay (s: state) =
//    let finished = s.workers |> Array.filter(fun x -> snd x = 0) |> Array.map fst
//    if (s.stps |> Array.isEmpty) then
//        if finished |> Array.isEmpty then 
//            let q = {workers = s.workers |> Array.map(fun (x,y) -> (x, y-1)); stps = s.stps; count = s.count + 1; Done = s.Done; Last = s.Last}
//            Some (q,q)    
//        else
//            None
//    else
//        let n = (finished |> Array.length)
//        let newstps = s.stps |> Array.filter(fun (x,y) -> finished |> Array.contains x |> not)
//        let order = (sift s.stps).ToCharArray() |> Array.take (h - n) 
//        let newwork = order|> Array.map(fun s -> (string s, delay + (int s) - (int 'A')))
//        
//        let q = { workers = Array.concat [|s.workers |> Array.filter(fun (x,y) -> y > 0); newwork|]; stps = newstps; count = s.count + 1; Done = s.Done + (finished |> String.concat ""); Last = s.Last}
//        Some(q,q)
let tlast = Set.difference (tst |> Array.map snd |> Set.ofArray) (tst |> Array.map fst |> Set.ofArray) 
let tini = {workers = [|("",0);("",0)|]; stps = tst; count = 0; Done = ""; Last = tlast}

let gogetem delay (s:state) =
    let doneTasks = s.workers |> Array.filter(fun (x,y) -> y = 0 && x <> "") |> Array.map fst
    let freeNumber = s.workers |> Array.filter(fun (x,y) -> y = 0) |> Array.length
    let occupiedWorkers = s.workers |> Array.filter(fun (x,y) -> y > 0) 
    if freeNumber = 0 then
        let q = {workers = s.workers |> Array.map(fun (x,y) -> (x,y-1)); stps = s.stps; count = s.count + 1; Done = s.Done; Last = s.Last}
        Some (q,q)
    else
        let doneT = doneTasks |> Array.sort |> String.concat ""
        let newStps = s.stps |> Array.filter(fun (x,y) -> doneTasks |> Array.contains x |> not )
        let prioritizedTasks = newStps |> Array.map fst |> Set.ofArray
        let secondaryTasks = newStps |> Array.map snd |> Set.ofArray
        let availableTasks = Set.difference (Set.difference prioritizedTasks secondaryTasks) (occupiedWorkers |> Array.map fst |> Set.ofArray) |> Array.ofSeq |> Array.sort
        let newTasksNumber = min (availableTasks |> Array.length) freeNumber
        let newTasks = availableTasks |> Array.take newTasksNumber |> Array.map(fun s -> (s, (int s.[0]) - (int 'A') + delay))
        let idleWorkers = Array.replicate ((s.workers |> Array.length) - newTasksNumber - (occupiedWorkers |> Array.length)) ("",0)
        if idleWorkers |> Array.length = (s.workers |> Array.length) && newStps |> Array.isEmpty then
            if s.Last.IsEmpty then
                None
            else
                let nt = s.Last |> Set.toArray |> Array.sort |> Array.take (min (idleWorkers |> Array.length) (s.Last.Count)) |> Array.map(fun s-> (s, (int s.[0]) - (int 'A') + delay))
                let iw = Array.replicate ((s.workers |> Array.length) - (nt |> Array.length)) ("",0)
                let q = {workers = Array.concat [|nt;iw|]; stps = newStps; count = s.count + 1; Done = s.Done + doneT; Last = Set.difference s.Last (nt |> Array.map fst |> Set.ofArray)}
                Some(q,q)
        else
            
            let doneT = s.workers |> Array.filter(fun (x,y) -> y = 0 && x <> "") |> Array.map fst |> Array.sort |> String.concat ""
            let q = {workers = Array.concat [|occupiedWorkers |> Array.map(fun (x,y) -> (x, y-1)); newTasks; idleWorkers|]; stps = newStps; count = s.count + 1; Done = s.Done + doneT ; Last = s.Last}
            Some(q,q)

Seq.unfold (gogetem 0) tini
|> Seq.length

Seq.unfold (gogetem 60) ini
|> Seq.length