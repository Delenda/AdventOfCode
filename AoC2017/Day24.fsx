let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day24.txt")

type comp= {front: int; back: int; id: int}

let comps = input |> Array.mapi(fun i s -> {id = i; front = s.Split('/').[0] |> int; back = s.Split('/').[1] |> int}) |> Set.ofArray

let init = comps |> Set.filter(fun d -> d.front = 0) |> Set.minElement

let flip s = {s with front = s.back; back = s.front}

let rec strongest comp candidates=
    let frontCandidates = candidates |> Set.filter(fun s -> s.front = comp.back)
    let backCandidates  = candidates |> Set.filter(fun s -> s.back  = comp.back)
    let a = 
        match frontCandidates.Count with
        | 0 -> 0
        |_ -> frontCandidates |> Set.map(fun s-> strongest s (candidates.Remove(s)) ) |> Seq.max 
    let b = 
        match backCandidates.Count with
        | 0 -> 0
        | _ -> backCandidates  |> Set.map(fun s-> strongest (flip s) (candidates.Remove(s)) ) |> Seq.max 
    comp.front + comp.back + (max a b)

let rec longest comp candidates=
    let frontCandidates = candidates |> Set.filter(fun s -> s.front = comp.back) |> Set.map(fun s -> longest s        (candidates.Remove(s))) |> Seq.sortDescending |> Array.ofSeq 
    let backCandidates =  candidates |> Set.filter(fun s -> s.back  = comp.back) |> Set.map(fun s -> longest (flip s) (candidates.Remove(s))) |> Seq.sortDescending |> Array.ofSeq

    let a = frontCandidates |> Array.length
    let b = backCandidates |> Array.length
    let r = (1, comp.front + comp.back)

    let res = 
        match a, b with
        | 0,0 -> r
        | 0,_ -> 
                let x = backCandidates |> Array.head
                (fst r + fst x, snd r + snd x)
        | _,0 -> 
                let x = frontCandidates |> Array.head
                (fst r + fst x, snd r + snd x)
        |_ -> 
                let x = frontCandidates |> Array.head
                let y = backCandidates |> Array.head
                let z = max x y
                (fst r + fst z, snd r + snd z)
    res

let question1 = strongest init comps
let question2 = longest init comps |> snd