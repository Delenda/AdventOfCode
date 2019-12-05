let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day08.txt") 

type operation = 
    | Rect of int*int
    | RotateRow of int*int
    | RotateColumn of int*int

let rect rows columns (screen: bool array array) = 
    screen 
    |> Array.mapi(fun row arr -> 
        arr 
        |> Array.mapi(fun col b -> 
            if row < rows && col < columns then true else b))

let rotateRow rowNr rotations (screen: bool array array) =
    screen
    |> Array.mapi(fun row arr -> 
        if row <> rowNr then arr else
            let first, last = arr |> Array.splitAt (arr.Length - rotations) 
            Array.append last first)

let rotateColumn colNr rotations (screen:bool array array) =
    screen
    |> Array.mapi(fun row arr -> 
        arr
        |> Array.mapi(fun col b -> 
            if col <> colNr then b else
                screen.[(row - rotations + screen.Length)%screen.Length].[col]))

let execute (screen: bool array array) op = 
    match op with
    | Rect(a,b) -> rect a b screen
    | RotateRow(a,b) -> rotateRow a b screen
    | RotateColumn(a,b) -> rotateColumn a b screen

let parse (s:string )=
    let rect = System.Text.RegularExpressions.Regex("rect (\d+)x(\d+)").Match s
    let col  = System.Text.RegularExpressions.Regex("rotate column x=(\d+) by (\d+)").Match s
    let row  = System.Text.RegularExpressions.Regex("rotate row y=(\d+) by (\d+)").Match s

    if rect.Success then
        let c = rect.Groups.[1].Value |> int
        let r = rect.Groups.[2].Value |> int
        Rect(r,c)
    else if col.Success then
        let c = col.Groups.[1].Value |> int
        let rot = col.Groups.[2].Value |> int
        RotateColumn(c,rot)
    else if row.Success then 
        let r = row.Groups.[1].Value |> int
        let rot = row.Groups.[2].Value |> int
        RotateRow(r,rot)
    else
        failwith (sprintf "no parse: %s" s)

let print (screen:bool array array)=
    screen
    |> Seq.map(Array.map(fun b -> if b then "#" else " "))
    |> Seq.map(String.concat "")
    |> String.concat "\r\n"
    |> (+) "\r\n"

let operations = 
    input
    |> Array.map parse

let blankScreen= Array.replicate 6 (Array.replicate 50 false)

let part1 = 
    operations 
        |> Seq.fold execute blankScreen
        |> Seq.map( Array.map(fun i -> if i then 1 else 0))
        |> Seq.map( Array.sum)
        |> Seq.sum

let part2 = 
    operations  
        |> Seq.scan execute blankScreen 
        |> Seq.map print 
        |> Seq.last