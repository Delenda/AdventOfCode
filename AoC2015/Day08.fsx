let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day08.txt")

let countLine (str:string) =
    let trim = str.Replace(@"\\", "-").Replace("\\\"","-")
    let xs = trim.Split([|"\\x"|],System.StringSplitOptions.None).Length - 1
    let inMemory = trim.Length - 3*xs - 2
    str.Length - inMemory

let encode (str:string) =
    let encoded = str.ToCharArray() |> Array.map(fun c -> match c with | '\\' -> "\\\\" | '"' -> "\\\"" | _ -> string c) |> String.concat ""
    2 + encoded.Length - str.Length
    
let question1 = 
    input |> Array.sumBy countLine

let question2 = 
    input |> Array.sumBy encode