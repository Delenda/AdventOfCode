let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day18.txt")

type assembler =    | Sti of char * int64
                    | Stc of char * char
                    | Snd of char
                    | Adi of char * int64
                    | Adc of char * char
                    | Mul of char * int64
                    | Mdc of char * char
                    | Mdi of char * int64
                    | Rcv of char
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
    | "snd" -> Snd(s.[1].[0])
    | "add" -> getChoice Adi Adc s
    | "mul" -> Mul(s.[1].[0], (s.[2]) |> int64)
    | "mod" -> getChoice Mdi Mdc s
    | "rcv" -> Rcv(s.[1].[0])
    | "jgz" -> match (s.[1].[0]) |> int > 90 with
                | true -> getChoice Jzi Jzc s
                | false -> Jmp((s.[2]) |> int64)
    | _ -> failwith("Unexpected")

type state = {Registers : Map<char,int64>; Position : int64; Song: int64 list}

let step (program : assembler array) (state : state) =
    let instruction = program.[state.Position |> int]
    let newRegister =
        match instruction with
        | Sti(a,b) -> state.Registers.Add(a,b)
        | Stc(a,b) -> state.Registers.Add(a,state.Registers.[b])
        | Adi(a,b) -> state.Registers.Add(a, state.Registers.[a] + b)
        | Adc(a,b) -> state.Registers.Add(a, state.Registers.[a] + state.Registers.[b])
        | Mdi(a,b) -> state.Registers.Add(a, state.Registers.[a] % b)
        | Mdc(a,b) -> state.Registers.Add(a, state.Registers.[a] % state.Registers.[b])
        | Mul(a,b) -> state.Registers.Add(a, state.Registers.[a] * b)
        | Jmp _ | Jzi _ | Jzc _ | Snd _ | Rcv _-> state.Registers

    let newPosition =
        match instruction with
        | Sti _| Stc _| Adi _| Adc _| Mdi _| Mdc _| Mul _| Snd _| Rcv _ -> state.Position + 1L
        | Jmp a -> state.Position + a
        | Jzi(a,b) -> if state.Registers.[a] > 0L then state.Position + b else state.Position + 1L
        | Jzc(a,b) -> if state.Registers.[a] > 0L then state.Position + state.Registers.[b] else state.Position + 1L

    let newSong =
        match instruction with
        | Snd a -> [state.Registers.[a]] @ state.Song
        | _ -> state.Song

    match newPosition, instruction with
    | a, _ when a < 0L || a > (program.Length |> int64) -> None
    | _, Rcv a when state.Registers.[a] > 0L -> None
    | _ ->

    let newState = {Registers = newRegister; Position = newPosition; Song = newSong}
    Some(newState.Song, newState)

let getRegister instruction =
    match instruction with
    | Sti(a,_) -> [a]
    | Stc(a,b) -> [a;b]
    | Snd a -> [a]
    | Adi (a,_) -> [a]
    | Adc (a,b) -> [a;b]
    | Mdi (a,_) -> [a]
    | Mdc (a,b) -> [a;b]
    | Jzi (a,_) -> [a]
    | Jzc (a,b) -> [a;b]
    | Mul (a,_) -> [a]
    | Rcv a -> [a]
    | Jmp _ -> []

let question1 str =
    let instructions = str |> Array.map parse
    let register = instructions |> Array.collect (getRegister >> Array.ofList) |> Array.distinct |> Array.map(fun s -> (s,0L)) |> Map.ofArray
    let finalstate = Seq.unfold (step instructions) {Registers = register; Position = 0L; Song = []} |> Seq.last
    finalstate.Head |> int

type programState = {Position : int64; Register: Map<char,int64>; Inbox : int64 list}

type bistate = {ProgramA : programState; ProgramB : programState; Sendcount : int}

let bistep (program : assembler array) bistate = 
    let instructionA = program.[bistate.ProgramA.Position |> int]
    let instructionB = program.[bistate.ProgramB.Position |> int]
    match instructionA, instructionB, bistate.ProgramA.Inbox, bistate.ProgramB.Inbox with
    | Rcv _, Rcv _, [], [] -> None
    | _ -> 
            let updateRegister state instruction =
                match instruction with
                | Sti(a,b) -> state.Register.Add(a,b)
                | Stc(a,b) -> state.Register.Add(a, state.Register.[b])
                | Adi(a,b) -> state.Register.Add(a, state.Register.[a] + b)
                | Adc(a,b) -> state.Register.Add(a, state.Register.[a] + state.Register.[b])
                | Mdi(a,b) -> state.Register.Add(a, state.Register.[a] % b)
                | Mdc(a,b) -> state.Register.Add(a, state.Register.[a] % state.Register.[b])
                | Mul(a,b) -> state.Register.Add(a, state.Register.[a] * b)
                | Rcv a    -> if state.Inbox.Length > 0 then state.Register.Add(a, state.Inbox.Head) else state.Register
                | Jmp _ | Jzi _ | Jzc _ | Snd _ -> state.Register
            let newRegisterA = updateRegister bistate.ProgramA instructionA
            let newRegisterB = updateRegister bistate.ProgramB instructionB

            let updatePosition state instruction =
                match instruction with
                | Sti _| Stc _| Adi _| Adc _| Mdi _| Mdc _| Mul _| Snd _ -> state.Position + 1L
                | Jmp a -> state.Position + a
                | Jzi(a,b) -> if state.Register.[a] > 0L then state.Position + b else state.Position + 1L
                | Jzc(a,b) -> if state.Register.[a] > 0L then state.Position + state.Register.[b] else state.Position + 1L
                | Rcv _ -> if state.Inbox.Length > 0 then state.Position + 1L else state.Position
            let newPositionA = updatePosition bistate.ProgramA instructionA
            let newPositionB = updatePosition bistate.ProgramB instructionB

            let newSendcount = 
                match instructionB with
                | Snd _ -> bistate.Sendcount + 1
                | _ -> bistate.Sendcount

            let sendValue instruction (inbox : int64 list) (register : Map<char,int64>) = 
                match instruction, inbox with
                | Snd a, xs -> xs @ [register.[a]]
                | _ -> inbox

            let receiveValue instruction inbox =
                match instruction, inbox with
                | Rcv a, b::xs -> xs
                | _  -> inbox

            let tmpInboxA = receiveValue instructionA bistate.ProgramA.Inbox
            let tmpInboxB = receiveValue instructionB bistate.ProgramB.Inbox

            let newInboxA = sendValue instructionB tmpInboxA bistate.ProgramB.Register
            let newInboxB = sendValue instructionA tmpInboxB bistate.ProgramA.Register

            let newProgramA  = {Position = newPositionA; Register = newRegisterA; Inbox = newInboxA}
            let newProgramB  = {Position = newPositionB; Register = newRegisterB; Inbox = newInboxB}
            let newBistate = {ProgramA = newProgramA; ProgramB = newProgramB; Sendcount = newSendcount} 

            match newPositionA, newPositionB with
            | a, _ when a < 0L || a > (program.Length |> int64) -> None
            | _, b when b < 0L || b > (program.Length |> int64) -> None
            | _ -> Some (newBistate.Sendcount, newBistate)

let question2 str =
    let instructions = str |> Array.map parse
    let registerA = instructions |> Array.collect (getRegister >> Array.ofList) |> Array.distinct |> Array.map(fun s -> (s,0L)) |> Map.ofArray
    let registerB = registerA.Add('p', 1L)
    let programA = {Position = 0L; Register = registerA; Inbox = [] }
    let programB = {Position = 0L; Register = registerB; Inbox = [] }
    let bistate = {ProgramA = programA; ProgramB = programB; Sendcount =  0}
    Seq.unfold (bistep instructions) bistate |> Seq.last

question1 input
question2 input