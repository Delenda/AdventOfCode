let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day15.txt") 

type sensorinfo = 
    {
        Sensor: int64*int64
        Beacon: int64*int64
    }

let parse (line:string) =
    let pattern = "Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
    let regex = System.Text.RegularExpressions.Regex(pattern)
    let m = regex.Match(line)
    let sensor_x = m.Groups.[1].Value |> int64
    let sensor_y = m.Groups.[2].Value |> int64
    let beacon_x = m.Groups.[3].Value |> int64
    let beacon_y = m.Groups.[4].Value |> int64
    {
        Sensor = sensor_x,sensor_y
        Beacon = beacon_x, beacon_y 
    }

let sensorinfo = input |> Array.map parse

let manhattanDist (x,y) (z,w) = abs(x-z) + abs(y-w)
   
let coveredInterval  (lineNr:int64) minX maxX (sensor:sensorinfo) = 
    let maxdist = manhattanDist sensor.Sensor sensor.Beacon
    let (x,y) = sensor.Sensor
    let yDist = abs(lineNr - y)
    if yDist > maxdist then None else
    let left  = max minX (x - (maxdist - yDist))
    let right = min maxX (x + (maxdist - yDist))
    Some(left,right)

let intervals minX maxX lineNumber= 
    sensorinfo
    |> Seq.choose (coveredInterval lineNumber minX maxX)
    |> Seq.toList

let compressedCoordinates intervals= 
    intervals 
    |> List.collect(fun (a,b) -> [a;b+1L]) 
    |> List.distinct
    |> List.sort
    |> List.indexed
    |> List.map(fun (a,b) -> b,a)
    |> Map

let compressedIntervalLengths compressedCoordinates= 
    compressedCoordinates
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.sort
    |> Seq.windowed 2
    |> Seq.mapi(fun i s -> i, (s|> Seq.last) - (s |> Seq.head))
    |> Map

let compressedIntervals intervals (compressedCoordinates:Map<int64,int>)= 
    intervals
    |> List.map(fun (a,b) -> [compressedCoordinates.[a]..compressedCoordinates.[b+1L]-1] |> Set)
    |> Set.unionMany
 
let coveredLength lineNumber minX maxX = 
    let intervals = intervals minX maxX lineNumber
    let compressedCoordinates = compressedCoordinates intervals
    let compressedIntervalLengths = compressedIntervalLengths compressedCoordinates
    let compressedIntervals = compressedIntervals intervals compressedCoordinates
    compressedIntervals |> Seq.sumBy(fun s -> compressedIntervalLengths.[s])

let part1 = 
    let lineNr = 2000000L
    let coveredPositions = coveredLength lineNr -1000000000000L 1000000000000L
    let beaconOnLine = sensorinfo |> Seq.map(fun s -> s.Beacon) |>  Seq.filter(fun s -> snd s = lineNr) |> Seq.distinct |> Seq.length |> int64
    coveredPositions - beaconOnLine

let sw = System.Diagnostics.Stopwatch.StartNew()
let part2 = 
    let minX = 0L
    let maxX = 4000000L
    let yCoordinate = //2639657L
        Seq.init (int maxX + 1) id
        |> Seq.chunkBySize 64
        |> Seq.collect(fun a -> a |> Seq.toArray |> Array.Parallel.map(fun i -> i,coveredLength (int64 i) minX maxX  ))
        |> Seq.filter(fun i -> snd i = maxX)
        |> Seq.head
        |> fst
    let intervals = intervals minX maxX (int64 yCoordinate)
    let compressedCoordinates = compressedCoordinates intervals
    let compressedIntervals = compressedIntervals intervals compressedCoordinates
    let aa = compressedCoordinates |> Map.toSeq |> Seq.map snd |> Seq.rev |> Seq.tail |> Set
    let xCoordinate = 
        let compressedCoordinate = (aa - compressedIntervals) |> Seq.exactlyOne
        compressedCoordinates |> Map.filter(fun k v -> v = compressedCoordinate) |> Map.toSeq |> Seq.exactlyOne |> fst
    4000000L*(int64 xCoordinate) + (int64 yCoordinate)

let duration = sw.Elapsed.TotalSeconds

