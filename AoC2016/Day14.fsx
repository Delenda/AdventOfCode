let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day14.txt")

let md5 = System.Security.Cryptography.MD5.Create()

let getHash salt = 
    md5
        .ComputeHash((salt.ToString()).ToCharArray() |> Array.map byte)
        |> Array.map(fun b -> b.ToString("x2")) 
        |> String.concat ""

let index salt stretching idx=
    let hashing d = 
        Seq.init stretching id
        |> Seq.scan(fun s _ -> getHash s) (getHash d)
        |> Seq.last
        
    let initial1000 = 
        Seq.initInfinite id
        |> Seq.map(fun idx -> idx,hashing (salt+idx.ToString()))
        |> Seq.take 1001
        |> Set
    
    Seq.initInfinite id
        |> Seq.scan(fun (next1000 : Set<int*string>) idx -> 
            let min = next1000.MinimumElement
            let nextHash = hashing (salt+(idx + 1000).ToString())
            next1000.Remove(min).Add (idx + 1000,nextHash)
            ) initial1000
        |> Seq.filter(fun next1000 ->
            let hash = next1000.MinimumElement |> snd
            let m = System.Text.RegularExpressions.Regex.Match(hash, @"([\w|\d])\1\1")
            m.Success)
        |> Seq.filter(fun next1000 -> 
            let hash = next1000.MinimumElement |> snd
            let c = System.Text.RegularExpressions.Regex.Match(hash, @"([\w|\d])\1\1").Groups.[1].Value
            let pattern = c+c+c+c+c
            next1000 |> Seq.tail |> Seq.exists(fun (idx,hash) -> hash.Contains pattern )
            )
        |> Seq.map(fun next1000 -> 
            let r = next1000.MinimumElement |> fst
            System.Console.WriteLine r
            r)
        |> Seq.take idx
        |> Seq.last

let part1 = index input 0 64
let part2 = index input 2016 64 //slow ~ 3 minutes