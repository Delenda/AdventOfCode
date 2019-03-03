let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day04.txt") |> Array.sort

let gettime (s:string) = System.DateTime.Parse(s.Substring(1,16))
let diff (ch: System.DateTime list) =
    let x = ch |> List.head
    let y = ch |> List.last
    y.Subtract(x).Minutes

let findTimes (fra,til)=
    [fra+1..til-1] 
        |> List.map(fun i -> input.[i]) 
        |> List.chunkBySize 2
        |> List.map(fun ch -> ch |> List.map gettime)
        |> List.map diff
        |> List.sum

let findGuard (s:string) =
    s.Split(' ').[3].Replace("#","") |> int

let getMinutes (ip:string array) ch=
    let a = ip.[ch |> List.head]
    let b = ip.[ch |> List.last]
    let cnt = diff [gettime a; gettime b] - 1
    let startMin = a.Substring(15,2) |> int
    [startMin..startMin+cnt] |> List.map(fun i -> i%60) |> Array.ofList

let genSleep (ip:string array) (fra,til)= 
    let guard = findGuard ip.[fra]
    [fra+1..til] 
        |> List.chunkBySize 2 
        |> List.map (getMinutes ip)
        |> List.map(fun x -> x |> Array.map(fun y -> (guard, y)))
        |> Array.ofList
        |> Array.collect id

let testinput = 
    [|
        "[1518-11-01 00:00] Guard #10 begins shift"
        "[1518-11-01 00:05] falls asleep"
        "[1518-11-01 00:25] wakes up"
        "[1518-11-01 00:30] falls asleep"
        "[1518-11-01 00:55] wakes up"
        "[1518-11-01 23:58] Guard #99 begins shift"
        "[1518-11-02 00:40] falls asleep"
        "[1518-11-02 00:50] wakes up"
        "[1518-11-03 00:05] Guard #10 begins shift"
        "[1518-11-03 00:24] falls asleep"
        "[1518-11-03 00:29] wakes up"
        "[1518-11-04 00:02] Guard #99 begins shift"
        "[1518-11-04 00:36] falls asleep"
        "[1518-11-04 00:46] wakes up"
        "[1518-11-05 00:03] Guard #99 begins shift"
        "[1518-11-05 00:45] falls asleep"
        "[1518-11-05 00:55] wakes up"
    |]

let shifts (ip:string array)= 
    let fst = 
        ip
        |> Array.indexed  
        |> Array.filter(fun x -> (snd x).Contains("Guard")) 
        |> Array.map fst
        |> List.ofArray
    fst @ [ip.Length-1] |> Array.ofList

let findSleep (ip : string array)= 
    shifts ip
    |> Array.pairwise
    |> Array.collect (genSleep ip)

let findtime (ip:string array) = 
    let sleep = findSleep ip
    let g = sleep |> Array.groupBy fst |> Array.map(fun (x,y) -> (x, y |> Array.length)) |> Array.maxBy snd |> fst
    let minute = sleep |> Array.filter(fun i -> fst i = g) |> Array.groupBy snd |> Array.maxBy(fun (x,y) -> y |> Array.length) |> fst    
    g * minute

findtime testinput

let maxmin (ip : string array) =
    let sleep = findSleep ip
    let g = sleep |> Array.groupBy id |> Array.maxBy(fun (x,y) -> y |> Array.length)
    (fst (fst g)) * (snd (fst g)) 

maxmin testinput

let part1 = 
    findtime input

let part2 = 
    maxmin input