let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt") 

let md5 = System.Security.Cryptography.MD5.Create()

let hash (s:string) =
    s.ToCharArray()
    |> Array.map byte
    |> md5.ComputeHash
    |> Array.map(fun b -> b.ToString("x2"))
    |> String.concat ""

let part1 = 
    Seq.initInfinite(sprintf "%s%d" input)
    |> Seq.map hash
    |> Seq.filter(fun s -> s.StartsWith ("00000"))
    |> Seq.map(fun s -> s.[5] |> string)
    |> Seq.take 8
    |> String.concat ""

let part2 = 
    Seq.initInfinite(sprintf "%s%d" input)
    |> Seq.map hash
    |> Seq.filter(fun s -> s.StartsWith("00000"))
    |> Seq.scan(fun (s:Map<int,char>) t -> 
        let key = int t.[5] - int '0'
        if s.ContainsKey key || key < 0 || key > 7 then s 
        else
            sprintf "%d %s" key (string t.[6]) |> System.Console.WriteLine
            s.Add(key, t.[6])) Map.empty
    |> Seq.filter(fun s -> s.Count = 8)
    |> Seq.head
    |> Seq.sortBy(fun k -> k.Key)
    |> Seq.map(fun k -> k.Value |> string)
    |> String.concat ""