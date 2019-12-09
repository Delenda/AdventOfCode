let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt") 

let runProgram given = 
    let program = input.Split(',') |> Array.map int
    let memmory = input.Split(',') |> Array.mapi (fun i x -> i, int x)    
    let read2Parameters instruction position = 
        let mode1 = (instruction/100)%10
        let mode2 = (instruction/1000)%10 
        let a = program.[position + 1]
        let b = program.[position + 2]
        let x = if mode1 = 0 then program.[a] else a
        let y = if mode2 = 0 then program.[b] else b
        x,y

    let rec execute position output= 
        let instruction = program.[position]
        let opcode = instruction % 100

        if opcode = 99 then
            output
        else if opcode = 1 || opcode = 2 then 
            let x, y = read2Parameters instruction position
            let c = program.[position + 3]
            let d = if opcode = 1 then x + y else x*y
            program.[c]<-d
            execute (position+4) output
        else if opcode = 3 then
            let a = program.[position + 1]
            program.[a]<-given
            execute (position+2) output
        else if opcode = 4 then
            let mode = (instruction/100)%10
            let a = program.[position + 1]
            let x = if mode = 0 then program.[a] else a
            execute (position+2) (x::output)
        else if opcode = 5 then
            let x,y = read2Parameters instruction position
            if x <> 0 then execute y output else execute(position+3) output
        else if opcode = 6 then
            let x,y = read2Parameters instruction position
            if x = 0 then execute y output else execute(position+3) output
        else if opcode = 7 then
            let x,y = read2Parameters instruction position
            let c = program.[position + 3]
            if x < y then program.[c]<-1 else program.[c]<-0
            execute (position+4) output
        else if opcode = 8 then
            let x,y = read2Parameters instruction position
            let c = program.[position + 3]
            if x = y then program.[c]<-1 else program.[c]<-0
            execute (position+4) output
        else failwith ("invalid opcode")
    execute 0 []

let part1 = 
    runProgram 1 |> List.head

let part2 = 
    runProgram 5 |> List.head