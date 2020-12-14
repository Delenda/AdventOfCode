open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day14.txt")

type instruction = 
    | Mask of Set<int> * Set<int>
    | Mem of int64 * int64

let parse (line:string) =
    if line.[1] = 'a' then
        let s = line.Split(' ') |> Array.last
        let on = 
            s.ToCharArray() 
            |> Array.rev 
            |> Array.indexed
            |> Array.filter(fun x -> snd x = '1')
            |> Array.map fst
        let off = 
            s.ToCharArray() 
            |> Array.rev 
            |> Array.indexed
            |> Array.filter(fun x -> snd x = '0')
            |> Array.map fst
        Mask (Set on, Set off)
    else
        let pattern = @"mem\[(\d+)\] = (\d+)"
        let m = Regex.Match(line, pattern)
        Mem (m.Groups.[1].Value |> int64, m.Groups.[2].Value |> int64)

type state = 
    {Memory : Map<int64,int64>; Mask_on : Set<int>; Mask_off : Set<int>}

let applyMasks (on:Set<int>) (off:Set<int>) (v:int64) = 
    System.Convert.ToString(v,2).PadLeft(36,'0').ToCharArray()
    |> Seq.rev
    |> Seq.indexed
    |> Seq.fold(fun t (i,c) ->
        if on.Contains i then 
            t + (pown 2L i)
        elif off.Contains i then
            t
        elif c ='1' then 
            t + (pown 2L i)
        else
            t
        ) 0L

let step state t = 
    match t with
    | Mask (on, off) -> {state with Mask_on = on; Mask_off = off}
    | Mem (address, v) -> {state with Memory = state.Memory.Add(address, applyMasks state.Mask_on state.Mask_off v)}

let part1 = 
    input
    |> Array.map parse
    |> Array.fold step {Memory = Map.empty; Mask_off = Set.empty; Mask_on = Set.empty}
    |> fun s -> s.Memory |> Seq.sumBy(fun kvp -> kvp.Value)

let addresses (address:int64) (on:Set<int>) (off:Set<int>) = 
    System.Convert.ToString(address, 2).PadLeft(36,'0').ToCharArray()
    |> Seq.toList
    |> Seq.rev
    |> Seq.toList
    |> List.indexed
    |> List.fold(fun s (i,c) -> 
        if on.Contains i then
            s |>List.map(fun t -> t + (pown 2L i))
        elif off.Contains i then
            if c = '1' then 
                s |>List.map(fun t -> t + (pown 2L i))
            else
                s
        else
            s |> List.collect(fun t -> [t; t + (pown 2L i)])) [0L]

let step2 (state:state) t = 
    match t with
    | Mask (on, off) -> {state with Mask_on = on; Mask_off = off}
    | Mem (address, v) -> 
        let newMem = 
            addresses address state.Mask_on state.Mask_off
            |> List.fold(fun (s:Map<int64,int64>) t -> s.Add(t, v)) (state.Memory)
        {state with Memory = newMem}

let part2 = 
    input
    |> Array.map parse
    |> Array.fold step2 {Memory = Map.empty; Mask_off = Set.empty; Mask_on = Set.empty}
    |> fun s -> s.Memory |> Seq.sumBy(fun kvp -> kvp.Value)