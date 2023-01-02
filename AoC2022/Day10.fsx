let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day10.txt") 

let registerValues = 
    input
    |> Seq.fold(fun values t ->
        let cycle = values |> List.head |> fst
        let register = values |> List.head |> snd
        if t.StartsWith("n") then
            [cycle + 1, register]@values
        else
            let v = t.Split(' ').[1] |> int
            [cycle + 2, register + v; cycle + 1, register]@values
        ) [1,1]
    |> Map

let part1  =
    [20;60;100;140;180;220] |> List.map(fun cycle -> registerValues.[cycle]*cycle) |> List.sum

let part2 = 
    Array.init 240 (fun i -> 
                        let crt = i%40
                        let sprite = registerValues.[i+1]
                        if abs(sprite - crt) <= 1 then "#" else ".")
    |> Array.chunkBySize 40
    |> Array.map (String.concat "")
    