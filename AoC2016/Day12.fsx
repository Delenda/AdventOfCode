let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day12.txt")

#load "Assembunny.fsx"

open Assembunny

let program = input |> Array.map parse

let part1 = run program (C,0L)
let part2 = run program (C,1L)