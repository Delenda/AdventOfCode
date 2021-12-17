let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day17.txt") 

type target = {MinX:int; MaxX:int; MinY:int; MaxY:int}

let parseTarget (str:string) = 
    let pattern = "target area: x=(\d+)..(\d+), y=-(\d+)..-(\d+)"
    let m = System.Text.RegularExpressions.Regex.Match(str, pattern)
    { MinX = m.Groups.[1].Value |> int; MaxX = m.Groups.[2].Value |> int; MinY = -(m.Groups.[3].Value |> int); MaxY = -(m.Groups.[4].Value |> int)}

let target = parseTarget input
let maxTime = (System.Math.Sqrt (float target.MaxX) |> int) - 2 * target.MinY + (System.Math.Sqrt (float (-target.MinY)) |> int)

let velocities = 
    [1..target.MaxX]
    |> Seq.collect(fun vx -> 
            [1..maxTime]
            |> Seq.scan(fun (velocity,position) _ -> 
                let newVelocity = if velocity > 0 then velocity - 1 else 0
                (newVelocity, position + velocity)) (vx,0)
            |> Seq.indexed
            |> Seq.filter(fun (time,(_,position)) -> position >= target.MinX && position <= target.MaxX)
            |> Seq.map( fun (time, _) -> vx,time))
    |> Seq.collect(fun (vx, time) -> 
        [target.MinY .. -(target.MinY)]
        |> List.filter(fun vy -> 
            let y = -(time-1)*(time)/2 + vy*time
            y <= target.MaxY && y>=target.MinY)
        |> List.map(fun vy -> vx,vy))
    |> Seq.distinct
    |> Seq.toList

let part1 = 
    velocities 
    |> Seq.map snd
    |> Seq.max
    |> fun x -> (x+1)*x/2

let part2 = velocities.Length