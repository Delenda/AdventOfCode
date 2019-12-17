let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ +   @"\Input\Day17.txt") 

#load "IntCode.fsx"

let part1 = 
    let scaffold = 
        IntCode.runProgram input []
        |> List.rev
        |> List.map(int >> char >> string)
        |> String.concat ""
        |> fun s -> s.Split('\n')
        |> Array.filter(fun s -> s.Length > 0)
        |> Array.map(fun s -> s.ToCharArray())

    scaffold
        |> Array.mapi(fun i row -> 
            if i = 0 || i = (scaffold.Length - 1) then 0 else
            row |> Array.mapi(fun j c -> 
                if c <> '#' || j = 0 || j = row.Length - 1 then 0 else
                if scaffold.[i].[j+1] = '#' && scaffold.[i].[j-1] = '#' && scaffold.[i+1].[j] = '#' && scaffold.[i-1].[j] = '#' then j*i else 0) |> Array.sum)
        |> Array.sum

// Path: R10,L8,R10,R4,L6,L6,R10,R10,L8,R10,R4,L6,R12,R12,R10,L6,L6,R10,L6,R12,R12,R10,R10,L8,R10,R4,L6,L6,R10,R10,L8,R10,R4,L6,R12,R12,R10
// A: L6,R12,R12,R10
// B: R10,L8,R10,R4
// C: L6,L6,R10

let part2 = 
    let modifiedProgram = 
        input.Split(',')
        |> Array.mapi(fun i x-> if i = 0 then "2" else x)
        |> String.concat ","

    let command= 
        "B,C,B,A,C,A,B,C,B,A\nL,6,R,12,R,12,R,10\nR,10,L,8,R,10,R,4\nL,6,L,6,R,10\nn\n".ToCharArray() 
        |> Array.map int64
        |> Array.toList
    
    IntCode.runProgram modifiedProgram command
    |> List.rev
    |> List.last