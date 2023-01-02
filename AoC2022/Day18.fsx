let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day18.txt") 

let droplets = 
    input
    |> Array.map(fun line -> 
        let pattern = @"(\d+),(\d+),(\d+)"
        let m = System.Text.RegularExpressions.Regex(pattern).Match(line)
        m.Groups.[1].Value |> int, m.Groups.[2].Value |> int, m.Groups.[3].Value |> int)
    |> Set
    
let surfaceArea (drops:Set<int*int*int>) = 
    let borderCount = 
        seq{for (x1,y1,z1) in drops do
            for (x2,y2,z2) in drops do
                if abs(x2-x1) + abs(y2-y1) + abs(z2-z1) = 1 then 
                    yield ()}
        |> Seq.length
    6*drops.Count - borderCount

let minX = droplets |> Seq.map(fun (x,_,_) -> x) |> Seq.min |> fun s -> s - 1
let maxX = droplets |> Seq.map(fun (x,_,_) -> x) |> Seq.max |> fun s -> s + 1
let minY = droplets |> Seq.map(fun (_,y,_) -> y) |> Seq.min |> fun s -> s - 1
let maxY = droplets |> Seq.map(fun (_,y,_) -> y) |> Seq.max |> fun s -> s + 1
let minZ = droplets |> Seq.map(fun (_,_,z) -> z) |> Seq.min |> fun s -> s - 1
let maxZ = droplets |> Seq.map(fun (_,_,z) -> z) |> Seq.max |> fun s -> s + 1

let cube = 
    seq{for x in [minX..maxX] do
        for y in [minY..maxY] do
        for z in [minZ..maxZ] do
            yield (x,y,z)}
    |> Set

let water= 
    let neighbours (seen:Set<int*int*int>) (x,y,z) = 
        seq{ yield (x-1,y,z)
             yield (x+1,y,z)
             yield (x,y-1,z)
             yield (x,y+1,z)
             yield (x,y,z-1)
             yield (x,y,z+1)}
        |> Seq.filter(cube.Contains)
        |> Seq.filter(droplets.Contains >> not)
        |> Seq.filter(seen.Contains >> not)
        |> Set
    
    let cubeSurface = 
        cube 
        |> Set.filter(fun (x,y,z) -> x = minX || x = maxX || y = minY || y = maxY || z = minZ || z = maxZ)
        
    Seq.unfold(fun ((frontier, seen): Set<int*int*int>*Set<int*int*int> ) -> 
            if frontier.IsEmpty then None else
            let newFrontier = frontier |> Seq.map (neighbours seen) |> Set.unionMany
            let newSeen = seen + newFrontier
            let newState = newFrontier, newSeen
            Some(newSeen, newState)) (cubeSurface,cubeSurface)
    |> Seq.last

let airpockets = cube - water - droplets

let part1 = surfaceArea droplets
let part2 = surfaceArea droplets - surfaceArea airpockets
