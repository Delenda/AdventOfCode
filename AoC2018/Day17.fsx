let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\Day17.txt")

type coords = {xFra:int; xTil:int; yFra:int;yTil:int}

let parseCoords (s:string)=
    let fields = s.Replace(", ","=").Replace("..","=").Split('=')
    let firstChar = fields.[0]
    let firstCoord = fields.[1] |> int
    let secondFrom = fields.[3] |> int
    let secondTo = fields.[4] |> int
    let xFra,xTil,yFra,yTil = 
        if firstChar = "x" then
            firstCoord,firstCoord, secondFrom,secondTo
        else
            secondFrom,secondTo,firstCoord,firstCoord
    {
        xFra = xFra
        xTil = xTil
        yFra = yFra
        yTil = yTil
    } 

type orientation = | Vertical | Horizontal
                
type strip =  {location:int; left:int; right:int; Orientation: orientation}

let parseStrips (s:string)=
    let fields = s.Replace(", ","=").Replace("..","=").Split('=')
    let firstChar = fields.[0]
    let firstCoord = fields.[1] |> int
    let secondFrom = fields.[3] |> int
    let secondTo = fields.[4] |> int
    let orientation = if firstChar = "x" then Vertical else Horizontal
    {location = firstCoord; left = secondFrom; right = secondTo; Orientation = orientation}

let testinput=
    [|
        "x=495, y=2..7"
        "y=7, x=495..501"
        "x=501, y=3..7"
        "x=498, y=2..4"
        "x=506, y=1..2"
        "x=498, y=10..13"
        "x=504, y=10..13"
        "y=13, x=498..504"
    |]

let createMap (strRep: string array) =
    let strips = 
        strRep
        |> Array.map parseStrips
    let coords =
        strRep
         |> Array.map parseCoords
         |> Array.collect(fun c -> [c.xFra..c.xTil] |> List.collect(fun i -> [c.yFra..c.yTil] |> List.map(fun j -> (i,j) )) |> List.toArray)
    
    let maxX = coords |> Array.map fst |> Array.max |> (+) 1
    let minX = coords |> Array.map fst |> Array.min |> fun x -> x - 1
    let maxY = coords |> Array.map snd |> Array.max 
    let minY = coords |> Array.map snd |> Array.min |> fun x -> x - 1

    let emptyMap = Array.replicate (maxY-minY+1) (Array.replicate (maxX-minX+1) ".")

    let premap = 
        strips
        |> Array.fold(fun s t -> 
                            if t.Orientation = Horizontal then
                                s |> Array.mapi(fun i k -> 
                                    if i + minY <> t.location then 
                                        k 
                                    else
                                        k |> Array.mapi(fun j r -> 
                                            let c = j + minX
                                            if c < t.left || c > t.right then
                                                r
                                            else
                                                "#" 
                                                )    
                                            )
                            else
                                s |> Array.mapi(fun i k -> 
                                    let c = i + minY
                                    if c < t.left || c > t.right then
                                        k
                                    else
                                        k |> Array.mapi(fun j r -> if j + minX = t.location then "#" else r)
                                                    )
    
                            ) emptyMap
        |> Array.mapi(fun i k -> if i <> 0 then k else k |> Array.mapi(fun j r -> if j + minX = 500 then "+" else r))
    let bottom = Array.replicate (maxX-minX+1) "o"
    Array.append premap [|bottom|]

let printMap (map: (string array) array) =
    map |> Array.map(String.concat "") |> String.concat "\r\n" |> System.Console.WriteLine

let testMap = createMap testinput
let problemMap = createMap input

type state = {verticals : Set<int*int>; map : string array array}

let unfolder (s:state) =
    if s.verticals.IsEmpty then
        None
    else
        let vertStart = s.verticals |> Seq.head
        let bottom =
            [
                [(snd vertStart) + 1] |> List.toSeq
                Seq.unfold(fun t -> 
                                let c = s.map.[t].[fst vertStart] 
                                if  c <> "." && c <> "+" then
                                    None
                                else
                                    let newt = t + 1
                                    Some(newt,newt)
                                ) ((snd vertStart) + 1)
            ] |> List.toSeq
            |> Seq.collect id
            |> Seq.last
            |> fun x -> x - 1

        let rec fillHorizontal vertStop (map:string array array) =  
            let horizontalUnfolder inc ( (edge,t) : (int*int) option * int) =
                if edge.IsSome then
                    None
                else if map.[vertStop+1].[t] = "." || (map.[vertStop+1].[t] = "|" && map.[vertStop].[t] = "|")  then
                    let q = Some (t,vertStop), t  
                    Some(q,q)
                else if map.[vertStop].[t] <> "." && map.[vertStop].[t] <> "|" then
                    None
                else
                    let q: (int*int) option * int = None, (t+inc)
                    Some(q,q)

            let newVertRight, right = 
                Seq.unfold (horizontalUnfolder (+1)) (None,fst vertStart)
                |> Seq.last

            let newVertLeft, left =
                Seq.unfold (horizontalUnfolder (-1)) (None, fst vertStart)
                |> Seq.last

            let newVerts = [newVertRight; newVertLeft] |> Seq.choose id |> Set

            let newMap = 
                    if newVerts.IsEmpty then
                        let line = map.[vertStop] |> Array.mapi(fun i r -> if i<=left || i >= right then r else "~")
                        map |> Array.mapi(fun i l -> if i = vertStop then line else l)
                    else
                        let line = map.[vertStop] |> Array.mapi(fun i r -> if (i<left || i > right) then r else if r = "." then "|" else r)
                        map |> Array.mapi(fun i l -> if i = vertStop then line else l)
            
            if newVerts.IsEmpty then
                fillHorizontal (vertStop - 1) newMap
            else
                newVerts, newMap |> Array.mapi(fun i l -> if i < (snd vertStart) || i > vertStop then l else (l |> Array.mapi(fun j r -> if j = fst vertStart then "|" else r)))

        let newVerts, newMap = 
            let c = s.map.[bottom+1].[fst vertStart]
            if c <> "o" && c <> "|" then
                fillHorizontal bottom s.map
            else
                s.verticals, s.map  |> Array.mapi(fun i l -> if i < (snd vertStart) || i > bottom then l else (l |> Array.mapi(fun j r -> if j = fst vertStart then "|" else r)))
                         
        let q = {verticals = (Set.union newVerts s.verticals) |> Set.remove vertStart; map = newMap}
        Some(newMap,q)

let teststate = {verticals = Set.empty.Add (6,0); map = testMap}
let problemstate = {verticals = Set.empty.Add (97,0); map = problemMap}

let part1 = 
    Seq.unfold unfolder problemstate
    |> Seq.last
    |> Array.skip 1
    |> Array.map(Array.filter(fun s -> s = "~" || s = "|"))
    |> Array.sumBy(fun s -> s.Length)

let part2 = 
    Seq.unfold unfolder problemstate
    |> Seq.last
    |> Array.skip 1
    |> Array.map(Array.filter(fun s -> s = "~" ))
    |> Array.sumBy(fun s -> s.Length)
