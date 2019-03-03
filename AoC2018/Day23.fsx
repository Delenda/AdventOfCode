let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\Day23.txt")

type location = {x:int64;y:int64;z:int64}
type nanobot = {loc:location;Radius:int64}
let parse (s:string) =
    let fields = s.Replace("pos=<","").Replace(">, ",",").Replace("r=","").Split(',')
    {
        loc = 
            {
                x = fields.[0] |> int64 
                y = fields.[1] |> int64
                z = fields.[2] |> int64
            }
        Radius = fields.[3] |> int64
    }
    
let dist a b = 
    int64 (abs(a.x - b.x)) +  int64 (abs(a.y - b.y)) + int64 (abs(a.z - b.z))
let botdist a b = dist a.loc b.loc

let nanobots = input |> Array.map parse

type cube = 
    {
        maxX : int64
        minX : int64
        maxY : int64
        minY : int64
        maxZ : int64
        minZ : int64
    }
    member this.MaxCorner = 
        {
            x = this.maxX
            y = this.maxY
            z = this.maxZ
        }
    member this.MinCorner = 
        {
            x = this.minX
            y = this.minY
            z = this.minZ
        }
    member this.botDist bot = 
        ((dist bot.loc this.MaxCorner) + (dist bot.loc this.MinCorner) - (dist this.MaxCorner this.MinCorner))/2L
    member this.Locations = 
        [this.minX..this.maxX]
        |> List.collect(fun x -> 
            [this.minY..this.maxY]
            |> List.collect(fun y -> 
                [this.minZ..this.maxZ]
                |> List.map(fun z -> {x = x; y = y; z = z})
            ))

let subDivideCube cube = 
    let midX = (cube.maxX + cube.minX)/2L
    let midY = (cube.maxY + cube.minY)/2L
    let midZ = (cube.maxZ + cube.minZ)/2L
    let xcoords = [(midX, cube.maxX); (cube.minX, midX)]
    let ycoords = [(midY, cube.maxY); (cube.minY, midY)]
    let zcoords = [(midZ, cube.maxZ); (cube.minZ, midZ)]
    xcoords
    |> List.collect(fun (x1,x2) -> 
        ycoords |> List.collect(fun (y1,y2) -> 
            zcoords |> List.map(fun (z1,z2) -> 
                { maxX = x2; minX = x1; maxY = y2; minY = y1; maxZ = z2; minZ = z1}
            )))

let surroundingCube bots = 
    let x = bots |> Array.map(fun b -> b.loc.x |> abs) |> Array.max
    let y = bots |> Array.map(fun b -> b.loc.y |> abs) |> Array.max
    let z = bots |> Array.map(fun b -> b.loc.z |> abs) |> Array.max 
    let r = max x (max y z)
    {
        maxX = r
        minX = -r
        maxY = r
        minY = -r
        maxZ = r
        minZ = -r
    }

let inRangeOfCube bots (cube:cube) = 
    bots |> Array.filter(fun b -> cube.botDist b <= b.Radius) |> Array.length

let rec findMaximalCoveragePosition bots (cube:cube) =
    if dist cube.MaxCorner cube.MinCorner < 4L then
        let m = 
            cube.Locations
            |> List.map(fun l -> inRangeOfCube bots {maxX = l.x; minX = l.x; maxY = l.y; minY = l.y; maxZ = l.z; minZ = l.z})
            |> List.max
        cube.Locations 
        |> List.filter(fun l -> inRangeOfCube bots {maxX = l.x; minX = l.x; maxY = l.y; minY = l.y; maxZ = l.z; minZ = l.z} = m)
        |> List.minBy(dist {x = 0L; y = 0L; z = 0L})
    else
        findMaximalCoveragePosition bots (subDivideCube cube |> List.maxBy (inRangeOfCube bots))

let part1 = 
    let bot = nanobots |> Array.maxBy(fun b -> b.Radius)
    nanobots |> Array.filter(fun b -> botdist bot b <= bot.Radius) |> Array.length

let part2 = 
    let surCube = surroundingCube nanobots
    let pos = findMaximalCoveragePosition nanobots surCube
    dist {x = 0L; y = 0L; z = 0L} pos