let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day02.txt")

let runProgram noun verb = 
    let program = input.Split(',') |> Array.map int
    program.[1]<-noun
    program.[2]<-verb
    let rec execute position = 
        let opcode = program.[position]
        if opcode = 99 then
            ()
        else
            let a = program.[position + 1]
            let b = program.[position + 2]
            let c = program.[position + 3]
            let x = program.[a]
            let y = program.[b]
            let d = if opcode = 1 then x + y else x*y
            program.[c]<-d
            execute (position+4)
    execute 0
    program.[0]

let part1 = runProgram 12 2

let part2 = 
    seq{for noun in [0..99] do
        for verb in [0..99] do
            yield noun, verb, runProgram noun verb}
    |> Seq.filter(fun (_,_,result) -> result = 19690720)
    |> Seq.head
    |> fun (noun,verb,c) -> 100*noun+verb