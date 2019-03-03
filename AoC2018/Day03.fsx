let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day03.txt")
type piece = {id:int; x:int; y:int; width:int;height:int}
let parsePiece (s:string)=
    let fields=  s.Split("#@:,x".ToCharArray()) |> Array.map(fun s -> s.Replace(" ",""))    
    {
        id = fields.[1] |> int
        x = fields.[2] |> int
        y = fields.[3] |> int
        width = fields.[4] |> int
        height = fields.[5] |> int 
    }
let pieces = input |> Array.map parsePiece

let generatePieces (p:piece) =
   Array.replicate p.width 0 
   |> Array.mapi(fun i t -> Array.replicate p.height 0 |> Array.mapi(fun j s -> (p.x + i, p.y + j))) 
   |> Array.collect id

let overlaps = 
    pieces 
    |> Array.collect generatePieces 
    |> Array.groupBy id 
    |> Array.filter(fun (x,y) -> y |> Array.length > 1) 
    |> Array.collect snd 
    |> Set.ofArray

let nonOverlapping (p:piece) =
    p
    |> generatePieces 
    |> Array.exists (fun s -> overlaps.Contains s) 
    |> not

let part1 = 
     overlaps |> Set.count

let part2 = 
    pieces 
    |> Array.filter nonOverlapping 
    |> Array.exactlyOne 
    |> fun p -> p.id