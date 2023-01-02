let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day21.txt") 

type expression = 
    | Constant of int64
    | Plus     of string * string
    | Minus    of string * string
    | Divide   of string * string
    | Multiply of string * string

let parse (monkey:string) =
    let f = monkey.Split(':')
    let expr (op:string) constructor = 
        if f.[1].Contains op then 
            let e = f.[1].Split(op.[0])
            let a = e.[0].Replace(" ","") 
            let b = e.[1].Replace(" ","") 
            Some(f.[0],constructor(a,b))
        else 
            None
    let op = 
        [ expr "+" Plus
          expr "-" Minus
          expr "*" Multiply
          expr "/" Divide]
        |> List.tryPick id
    if op.IsSome then op.Value else f.[0],Constant (f.[1].Replace(" ","") |> int64)

let monkeys = input  |> Array.map parse |> Map

let rec evaluate (monkey:string) = 
   match monkeys.[monkey] with
   | Constant a     -> a
   | Plus     (a,b) -> evaluate a + evaluate b
   | Minus    (a,b) -> evaluate a - evaluate b
   | Divide   (a,b) -> evaluate a / evaluate b
   | Multiply (a,b) -> evaluate a * evaluate b
   
let rec containsHuman (name:string)= 
    if name= "humn" then true else
    match monkeys.[name] with
    | Constant _-> false
    | Plus(a,b) | Minus(a,b)| Multiply(a,b)| Divide(a,b) -> containsHuman a || containsHuman b

let rec solve (name:string) target = 
    if name = "humn" then target else
    match monkeys.[name] with
    | Constant _-> failwith "unexpected"
    | Plus     (a,b) when containsHuman b -> solve b (target - evaluate a)
    | Plus     (a,b)                      -> solve a (target - evaluate b)
    | Minus    (a,b) when containsHuman b -> solve b (evaluate a - target)
    | Minus    (a,b)                      -> solve a (target + evaluate b)
    | Divide   (a,b) when containsHuman b -> solve b (evaluate a / target)
    | Divide   (a,b)                      -> solve a (target * evaluate b)
    | Multiply (a,b) when containsHuman b -> solve b (target / evaluate a)
    | Multiply (a,b)                      -> solve a (target / evaluate b)
        
let part1 = evaluate "root"
let part2 = 
    let solveStart,target = 
        match monkeys.["root"] with
        | Constant _-> failwith "unexpected"
        | Plus(a,b) | Minus(a,b)| Multiply(a,b)| Divide(a,b) -> 
            if containsHuman a then a,evaluate b else b,evaluate a    
    solve solveStart target