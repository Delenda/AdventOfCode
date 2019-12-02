let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day02.txt") 

type move = |Up|Down|Left|Right
type position = 
    |One|Two|Three|Four|Five|Six|Seven|Eight|Nine|A|B|C|D
        override this.ToString() = 
            match this with
            |One -> "1"
            |Two -> "2"
            |Three -> "3"
            |Four -> "4"
            |Five -> "5"
            |Six -> "6"
            |Seven -> "7"
            |Eight -> "8"
            |Nine -> "9"
            | A -> "A"
            | B -> "B"
            | C -> "C"
            | D -> "D"

let moveSq position move= 
    match move, position with
    | Up, One | Left, One | Up, Four | Left, Two -> One
    | Up, Two | Left, Three | Up, Five | Right, One-> Two
    | Up, Three | Right, Three | Right, Two | Up, Six-> Three
    | Left, Four | Down, One | Up, Seven | Left, Five-> Four
    | Down, Two | Right, Four | Up, Eight | Left, Six -> Five
    | Down, Three | Right, Five | Up, Nine | Right, Six-> Six
    | Down, Seven | Left, Seven | Down, Four | Left, Eight -> Seven
    | Down, Eight | Right, Seven | Down, Five | Left, Nine -> Eight
    | Right, Nine | Down, Nine | Down, Six | Right, Eight-> Nine
    | _ -> failwith("Invalid combination")

let moveDiamond position move = 
    match move, position with
    | Left, One | Up, One | Right, One | Up, Three -> One
    | Up, Two | Left, Two | Left, Three | Up, Six -> Two
    | Down, One | Right, Two | Left, Four | Up, Seven -> Three
    | Right, Four | Up, Four | Right, Three | Up, Eight -> Four
    | Up, Five | Left, Five | Down, Five | Left, Six -> Five
    | Down, Two | Right, Five | Up, A | Left, Seven -> Six
    | Down, Three | Right, Six | Up, B | Left, Eight -> Seven
    | Down, Four | Right, Seven | Up, C | Left, Nine -> Eight
    | Up, Nine | Right, Nine | Down, Nine | Right, Eight -> Nine
    | Left, A | Down, A | Down, Six | Left, B -> A
    | Down, Seven | Right, A | Up, D | Left, C -> B
    | Right, C | Down, C | Right, B | Down, Eight -> C
    | Left, D | Down, D | Right, D | Down, B -> D

let parseChar = function   
    | 'L' -> Left
    | 'R' -> Right
    | 'U' -> Up
    | 'D' -> Down
    | _ -> failwith ("unexpected char")

let applyMoves mover startPosition moves = moves |> Seq.fold mover startPosition
let findCode mover = 
    input 
        |> Array.map(fun s -> s.ToCharArray() |> Array.map parseChar)
        |> Array.scan(fun s t -> applyMoves mover s t) Five
        |> Array.tail
        |> Array.map(fun s -> s.ToString())
        |> String.concat ""

let part1 = findCode moveSq
let part2 = findCode moveDiamond