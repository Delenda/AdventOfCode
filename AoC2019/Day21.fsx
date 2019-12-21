let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ +   @"\Input\Day21.txt") 

#load "IntCode.fsx"

let print (k : int64 list) =
    k
    |> List.rev
    |> List.map(char>>string)
    |> String.concat ""

let RunSpringScript script = 
    script  
    |> String.concat "\n"
    |> fun s -> s.ToCharArray() 
    |> Array.map int64
    |> Array.toList
    |> IntCode.runProgram input
    
let part1 = 
    [|
        "NOT C J"
        "NOT D T"
        "NOT T T"
        "AND T J"
        "NOT A T"
        "OR T J"
        "WALK\n"
    |]
    |> RunSpringScript
    |> List.head

let script = 
    [|
        "NOT A J"
        "NOT B T"
        "OR T J"
        "NOT C T"
        "OR T J"
        "AND D J"
        "NOT I T"
        "NOT T T"
        "OR F T"
        "AND E T"
        "OR H T"
        "AND T J"
        "RUN\n"
    |]
    |> RunSpringScript
    |> List.head