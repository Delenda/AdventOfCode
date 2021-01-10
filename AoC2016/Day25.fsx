let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day25.txt") 

#load "Assembunny.fsx"

open Assembunny

let program = input |> Array.map parse

let seed = 2048L + 512L + 128L + 32L + 8L + 2L - 14L * 182L 
//run program (A,seed)

// The input program takes the value in register a and adds 14*182 
// It then goes into an infinite loop that prints the binary representation
// of the result. 
let part1 = seed