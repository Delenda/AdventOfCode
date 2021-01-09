let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__+ @"\Input\Day23.txt") 

#load "Assembunny.fsx"

open Assembunny

let program = 
    input 
    |> Array.map parse

let part1 = run program (A,7L) 

let part2 = 
    let rec fac n = 
        if n = 1L then 1L else n * fac (n-1L)
    
    let modifiedProgram = 
        program
        |> Array.mapi(fun i opcode -> 
            if i = 0 then
                Cpy  (Litteral (fac 12L),Register A)
            else if i = 1 then
                Jnz (Litteral 1L, Litteral 15L)
            else if i = 24 || i = 22 || i = 20 || i =18 
                then toggle opcode 
            else
                opcode
            )
    run modifiedProgram (A,0L)