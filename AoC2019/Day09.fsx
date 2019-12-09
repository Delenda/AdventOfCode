let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day09.txt") 

#load "IntCode.fsx"

let test1 = IntCode.runProgram "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" [] |> List.rev = [109L;1L;204L;-1L;1001L;100L;1L;100L;1008L;100L;16L;101L;1006L;101L;0L;99L]
let test2 = IntCode.runProgram "1102,34915192,34915192,7,4,7,99,0" [] =  [1219070632396864L]
let test3 = IntCode.runProgram "104,1125899906842624,99" []= [1125899906842624L]

let part1 = IntCode.runProgram input [1L] |> Seq.head
let part2 = IntCode.runProgram input [2L] |> Seq.head

