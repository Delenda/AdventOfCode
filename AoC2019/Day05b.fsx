let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day05.txt") 

#load "IntCode.fsx"
    
let part1 = IntCode.runProgram input [1L] |> Seq.head
let part2 = IntCode.runProgram input [5L] |> Seq.head