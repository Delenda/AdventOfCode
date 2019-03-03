let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day23.txt")

type assembler =    | Sti of char * int64
                    | Stc of char * char
                    | Mli of char * int64
                    | Mlc of char * char
                    | Sbi of char * int64
                    | Sbc of char * char
                    | Jzc of char * char
                    | Jzi of char * int64
                    | Jmp of int64

let getChoice (ifunc : char*int64 -> assembler) (cfunc: char*char -> assembler) (s : string array) =
    let a = s.[2].[0]
    match a |> int > 90 with
    | true -> cfunc(s.[1].[0], s.[2].[0])
    | false ->ifunc(s.[1].[0], s.[2] |> int64)

let parse (line : string) =
    let s = line.Split(' ')
    match s.[0] with
    | "set" -> getChoice Sti Stc s
    | "mul" -> getChoice Mli Mlc s
    | "sub" -> getChoice Sbi Sbc s
    | "jnz" -> match (s.[1].[0]) |> int > 90 with
                | true -> getChoice Jzi Jzc s
                | false -> Jmp((s.[2]) |> int64)
    | _ -> failwith("Unexpected")

type state = {Registers : Map<char,int64>; Position : int64; Count : int}

let step (program : assembler array) (state : state) =
    let instruction = program.[state.Position |> int]
    let newRegister =
        match instruction with
        | Sti(a,b) -> state.Registers.Add(a,b)
        | Stc(a,b) -> state.Registers.Add(a,state.Registers.[b])
        | Mlc(a,b) -> state.Registers.Add(a, state.Registers.[a] * state.Registers.[b])
        | Mli(a,b) -> state.Registers.Add(a, state.Registers.[a] * b)
        | Sbc(a,b) -> state.Registers.Add(a, state.Registers.[a] - state.Registers.[b])
        | Sbi(a,b) -> state.Registers.Add(a, state.Registers.[a] - b)
        | Jmp _ | Jzi _ | Jzc _ -> state.Registers

    let newPosition =
        match instruction with
        | Sti _| Stc _| Sbi _| Sbc _| Mlc _ | Mli _ -> state.Position + 1L
        | Jmp a -> state.Position + a
        | Jzi(a,b) -> if state.Registers.[a] <> 0L then state.Position + b else state.Position + 1L
        | Jzc(a,b) -> if state.Registers.[a] <> 0L then state.Position + state.Registers.[b] else state.Position + 1L

    let newCount = 
        match instruction with
        | Mlc _ | Mli _ -> state.Count + 1
        | _ -> state.Count

    match newPosition with
    | a when a < 0L || a >= (program.Length |> int64) -> None
    | _ ->
    
    let newState = {Registers = newRegister; Position = newPosition; Count = newCount}
    Some(newState, newState)

let getRegister instruction =
    match instruction with
    | Sti(a,_) -> [a]
    | Stc(a,b) -> [a;b]
    | Mli(a,_) -> [a]
    | Mlc(a,b) -> [a;b]
    | Sbi(a,_) -> [a]
    | Sbc(a,b) -> [a;b]
    | Jzi (a,_) -> [a]
    | Jzc (a,b) -> [a;b]
    | Jmp _ -> []

let question1 str =
    let instructions = str |> Array.map parse
    let register = instructions |> Array.collect (getRegister >> Array.ofList) |> Array.distinct |> Array.map(fun s -> (s,0L)) |> Map.ofArray
    let finalstate = Seq.unfold (step instructions) {Registers = register; Position = 0L; Count = 0} |> Seq.last
    finalstate.Count
// Pseudocode:
//
//if a <> 0 then
//		b = 67 * 100 + 100000
//		c = b + 17000
//	else
//		b = 67
//		c = 67
//A:	f = 1
//	    d = 2
//C:	e = 2
//B:	if d*e = b then 
//		    f = 0
//	e++
//	if e <> b then goto B
//	d++
//	if d <> b then goto C
//	if f = 0 then
//		h++
//	if b = c then break
//	b = b + 17
//	goto A

let question2 str=
    let instructions = str |> Array.map(fun s -> match s with | "set f 1" -> "jnz 1 1000" | _ -> s) |> Array.map parse
    let register = instructions |> Array.collect (getRegister >> Array.ofList) |> Array.distinct |> Array.map(fun s -> (s,0L)) |> Map.ofArray
    let finalstate = Seq.unfold (step instructions) {Registers = register.Add('a',1L); Position = 0L; Count = 0} |> Seq.last

    let b = finalstate.Registers.['b'] |> int
    let c = finalstate.Registers.['c'] |> int
    
    let factors n = seq {
        for d in 1 .. (float >> sqrt >> int) n do
        if n % d = 0 then
            yield d
            if n / d <> d then yield n/d
    }

    let isPrime n =
        (factors n  |> Seq.length) = 2

    [b..17..c] |> List.filter (isPrime >> not) |> List.length

question1 input
question2 input


