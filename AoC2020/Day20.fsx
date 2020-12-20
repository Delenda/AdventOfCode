let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day20.txt")

let strRevert (s:string) = s.ToCharArray()|> Seq.rev |> Seq.map string |> String.concat ""

type border = 
    { Top : string; Bottom:string; Left:string; Right:string}
    member this.Flip = 
        {Top = this.Bottom; Bottom = this.Top; Left = strRevert this.Left; Right = strRevert this.Right}
    member this.Rotate =
        {Top = this.Left; Right = this.Top; Bottom = this.Right; Left = this.Bottom }
    member this.toSet = 
        [this.Top;this.Right;this.Bottom;this.Left] |> Set

type Tile = string array
let print (tile:Tile) = tile |> Seq.iter(System.Console.WriteLine)

type orientation = Tile -> string

let flip (tile:Tile) = 
    tile |> Array.mapi(fun i s -> s.ToCharArray() |> Array.mapi(fun j c -> tile.[j].[i] |> string) |> String.concat "")
let rotate = Array.rev >> flip
let dihedralGroup = [id; rotate; rotate >> rotate; rotate >> rotate >> rotate; flip; flip >> rotate; flip >> rotate >> rotate; flip >> rotate >> rotate >> rotate]

let TopBorder = Array.head
let BottomBorder = Array.rev >> Array.head
let LeftBorder (tile:string array)= tile |> Array.map(fun x -> x.[0] |> string) |> String.concat ""
let RightBorder (tile:string array)= tile |> Array.map(fun x -> x.[x.Length - 1] |> string) |> String.concat ""

let border (tile:Tile) = 
    let top = TopBorder tile
    let bottom = BottomBorder tile
    let left = LeftBorder tile
    let right = RightBorder tile
    {Top = top; Bottom = bottom; Left = left; Right = right}

let possibleBorders (tile:Tile) = 
    let border = border tile
    seq{
        yield border.Top
        yield border.Bottom
        yield border.Left
        yield border.Right
        yield border.Flip.Left
        yield border.Flip.Right
        yield border.Rotate.Flip.Left
        yield border.Rotate.Flip.Right} 
    |> Set

let tiles : Map<int,Tile>= 
    input.Split([|"\r\n\r\n"|], unbox 0)
    |> Array.map(fun s -> 
        let lines = s.Split([|"\r\n"|],unbox 0)
        let tilenumber = System.Text.RegularExpressions.Regex.Match(lines.[0], "Tile (\d+):").Groups.[1].Value |> int
        tilenumber, Array.tail lines)
    |> Map

let links = 
    tiles
    |> Map.toSeq
    |> Seq.collect(fun (tileId, tile) -> possibleBorders tile |> Seq.map(fun d -> d,tileId) )
    |> Seq.groupBy fst
    |> Seq.map snd
    |> Seq.filter(fun s -> s |> Seq.length > 1)
    |> Seq.collect(fun s -> 
        let first = Seq.head s |> snd
        let last = Seq.last s |> snd
        [first,last;last,first])
    |> Seq.groupBy fst
    |> Seq.map(fun (k,v) -> k, Set (v |> Seq.map snd))
    |> Map

let corners =
    links
    |> Map.toSeq
    |> Seq.filter(fun (_, neighbors) -> neighbors.Count = 2)
    |> Seq.map fst

let part1 = 
    corners
    |> Seq.map uint64
    |> Seq.reduce (*)

let part2 = 
    let rotations (connectingBorder : orientation) tile = 
        dihedralGroup
        |> Seq.map(fun symmetry -> symmetry tile)
        |> Seq.map(fun reorientedTile -> connectingBorder reorientedTile, reorientedTile)

    let cornerId = corners |> Seq.skip 2 |> Seq.head
    let cornerTile = tiles.[cornerId]

    let neighbor (connectingBorderSource : orientation) (connectingBorderTarget : orientation) (tileId, sourceTile)= 
        let sourceBorder = connectingBorderSource sourceTile
        links.[tileId] 
        |> Seq.collect(fun tileid -> 
            rotations connectingBorderTarget tiles.[tileid] |> Seq.map(fun d -> tileid, d))
        |> Seq.filter(fun (_,(targetBorder, _)) -> sourceBorder = targetBorder )
        |> Seq.exactlyOne
        |> fun (a,(b,c)) -> a,c

    let crop (tile:Tile) =
        tile |> Array.tail |> Array.rev |> Array.tail |> Array.rev |> Array.map(fun s -> s.Substring(1, s.Length - 2))
    
    let image_with_ids = 
        let length = tiles.Count |> float |> System.Math.Sqrt |> int
        [1..length - 1] 
        |> List.scan(fun s _ -> neighbor BottomBorder TopBorder s) (cornerId, cornerTile)
        |> List.map(fun leftTile -> [1..length - 1] |> List.scan(fun s _ -> neighbor RightBorder LeftBorder s) leftTile)
    
    let image = 
        image_with_ids
        |> List.map (List.map snd)
        |> List.map List.toArray
        |> List.toArray
        |> Array.map (Array.map crop)
        |> Array.collect(fun tilearray -> 
            [|0..7|] |> Array.map(fun row -> tilearray |> Array.map(fun t -> t.[row]) |> String.concat ""))

    let seamonster = 
        [
            "                  # " 
            "#    ##    ##    ###"
            " #  #  #  #  #  #   "  
        ]
    let pattern =  "(" + (seamonster |> Seq.map(fun s -> s.Replace(" ",".")) |> String.concat ( sprintf ".{%d}" (image.[0].Length-20))) + ")"

    let countSeamonsters (lines:string array) = 
        System.Text.RegularExpressions.Regex.Matches(lines |> String.concat "", pattern).Count

    let roughness img = 
        let count = dihedralGroup |> List.map(fun symmetry -> symmetry img) |> List.map countSeamonsters |> List.max
        let waves = img |> String.concat "" |> fun s -> s.ToCharArray() |> Seq.filter(fun c -> c = '#') |> Seq.length
        let sm = seamonster |> String.concat "" |> fun s -> s.Replace(" ","").Length
        waves - count * sm

    let transform = 
        dihedralGroup |> List.map(fun sym -> sym image) |> List.maxBy countSeamonsters
        
    let scanned (t:string array) =     
        let offsets = 
            seamonster 
            |> List.mapi(fun i s -> 
                s.ToCharArray() |> Seq.indexed |> Seq.filter(fun (_,c) -> c = '#') |> Seq.map (fun (j,c) -> j + (transform.Length)*i) |> Seq.toList )
            |> List.collect id

        let smidx =
            seq{ for m in System.Text.RegularExpressions.Regex.Matches(t |> String.concat "", pattern) do yield m.Index} 
            |> Seq.collect(fun idx -> offsets |> List.map(fun o -> idx + o))
            |> Set
        t |> String.concat "" |> fun s -> s.ToCharArray() |> Array.mapi(fun i c -> if smidx.Contains i then 'O' else c) |> Seq.map string |> Seq.chunkBySize (t.Length) |> Seq.map(String.concat "") |> Seq.toArray

    Seq.unfold(fun s -> 
        if countSeamonsters s = 0 then None else
        let newS= scanned s
        Some(newS, newS)
        ) transform
    |> Seq.last
print part2