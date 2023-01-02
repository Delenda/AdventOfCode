let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day22.txt") 

let movements = input |> Seq.last
let map = input |> Seq.rev |> Seq.skip 2 |> Seq.rev |> Seq.toArray
let mapWidth = map |> Seq.map(fun s -> s.Length) |> Seq.max

let rightBorder = map |> Seq.mapi(fun i s -> i, s.Length - 1) |> Map
let leftBorder = map |> Seq.mapi(fun i s -> i,s.ToCharArray() |> Seq.takeWhile(fun c -> c = ' ') |> Seq.length) |> Map
let verticalBorder pick = 
    seq{ for col in [0.. (mapWidth-1)] do
            let b =
                map
                |> Seq.indexed
                |> Seq.filter( fun (_,s) -> s.Length > col)
                |> Seq.filter(fun (_,s) -> s.[col] <> ' ')
                |> Seq.map fst
                |> pick
            yield col,b
            } |> Map
let upperBorder = verticalBorder Seq.min
let lowerBorder = verticalBorder Seq.max

type command = 
    | Forward of int
    | Right
    | Left

let parseCommand (tokens:char list) = 
    match tokens with
    | 'R'::_ -> Right, tokens.Tail
    | 'L'::_ -> Left, tokens.Tail
    | _ -> 
        let digits = tokens |> List.takeWhile(System.Char.IsDigit)
        digits |> Seq.map string |> String.concat "" |> int |> Forward, tokens |> List.skip digits.Length

let rec parseCommands (tokens:char list) commandList= 
    if tokens.IsEmpty then commandList else
    let cmd,rest = parseCommand tokens
    parseCommands rest (cmd::commandList)
    
let commands = parseCommands (movements.ToCharArray() |> Seq.toList) [] |> List.rev

let move (x,y,dir) =
    let newX,newY = 
        match dir with 
        | 0 -> 
            if x = rightBorder.[y] then 
                leftBorder.[y],y
            else
                x + 1,y
        | 2 ->
            if x = leftBorder.[y] then 
                rightBorder.[y],y
            else
                x - 1,y
        | 1 -> 
            if y = lowerBorder.[x] then
                x, upperBorder.[x]
            else
                x, y + 1
        | 3 -> 
            if y = upperBorder.[x] then
                x, lowerBorder.[x]
            else
                x, y - 1            
        | _ -> failwith "unexpected dir"
    if map.[newY].[newX] <> '#' then 
        let q = newX,newY,dir
        Some(q,q)
    else 
        let q = x,y,dir
        Some(q,q)

let step ((x,y,dir):int*int*int) (cmd:command) =
    match cmd with
    | Right -> x,y,(dir + 1)%4
    | Left  -> x,y,(dir + 3)%4
    | Forward n -> 
        Seq.unfold move (x,y,dir) |> Seq.take n |> Seq.last

let face x y = 
    if x >= 100 && y<50 then 4 else
    if x >= 50  && y<50 then 1 else
    if y < 100 then 2 else
    if x < 50 && y < 150 then 3 else
    if y < 150 then 6 else
    if x < 50 && y >= 150 then 5 
    else
        failwith "unexpected face location"

let moveCube (x,y,dir) cmd = 
    let newX, newY, newDir =
        match cmd with
        | Right -> x,y,(dir + 1)%4
        | Left  -> x,y,(dir + 3)%4
        | Forward 1 -> 
            match dir with
            | 0 -> 
                if x = rightBorder.[y] then 
                    match face x y with 
                    | 2 ->  50 + y,49,3
                    | 6 ->  149, 149-y, 2
                    | 4 ->  99, 149-y,2
                    | 5 ->  y - 100,149, 3
                    | _ -> failwith "unexpected face going right"
                else
                    x + 1,y,dir
            | 1 -> 
                if y = lowerBorder.[x] then
                    match face x y with 
                    | 4 -> 99,x-50, 2
                    | 6 -> 49,100+x, 2
                    | 5 -> x+100,0,1
                    | _ -> failwith "unexpected face going down"
                else
                    x, y + 1,dir
            | 2 -> 
                if x = leftBorder.[y] then 
                    match face x y with
                    | 2 -> y-50,100,1
                    | 1 -> 0,149-y,0
                    | 3 -> 50,149-y,0
                    | 5 -> y-100,0,1
                    | _ -> failwith "unexpected face going left"
                else
                    x - 1,y,dir
            | 3 -> 
                if y = upperBorder.[x] then
                    match face x y with
                    | 3 -> 50,50+x,0
                    | 1 -> 0,x+100,0
                    | 4 -> x-100,199,3
                    | _ -> failwith "unexpected face going up"
                else
                    x, y - 1,dir      
            | _ -> failwith "unexpected dir"
        | _ -> failwith "unexpected command"
    if map.[newY].[newX] <> '#' then 
        newX,newY,newDir
    else 
        x,y,dir

let part1 = 
    let start_x = upperBorder |> Map.toSeq |> Seq.filter(fun (x,y) -> y = 0) |> Seq.map fst |> Seq.min
    let start_y = 0
    let start_dir = 0
    let end_x, end_y,end_dir = commands |> List.fold step (start_x,start_y,start_dir)
    1000*(end_y+1) + 4*(end_x+1) + end_dir
    
let part2 = 
    let start_x = upperBorder |> Map.toSeq |> Seq.filter(fun (x,y) -> y = 0) |> Seq.map fst |> Seq.min
    let start_y = 0
    let start_dir = 0
    let expandedCommands = commands|> List.collect(fun c -> match c with | Right |Left -> [c] | Forward n -> List.replicate n (Forward 1))
    let end_x, end_y,end_dir = expandedCommands |> List.fold moveCube (start_x,start_y,start_dir)
    1000*(end_y+1) + 4*(end_x+1) + end_dir