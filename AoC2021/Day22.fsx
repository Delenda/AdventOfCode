let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day22.txt") 

type cube = {x_min:int; x_max:int; y_min:int;y_max:int; z_min:int;z_max:int}
type reboot_step = {isOn:bool; cube:cube}

let parse (line:string) =
    let on = line.StartsWith "on"
    let m = System.Text.RegularExpressions.Regex.Match( line, "x=(-*\d+)..(-*\d+),y=(-*\d+)..(-*\d+),z=(-*\d+)..(-*\d+)")
    let getint (n:int) = m.Groups.[n].Value |> int
    let x_min = getint 1
    let x_max = getint 2
    let y_min = getint 3
    let y_max = getint 4
    let z_min = getint 5
    let z_max = getint 6
    {isOn = on; cube = {x_max = x_max; x_min = x_min; y_max = y_max; y_min = y_min; z_max = z_max; z_min = z_min}}

let steps = input |> Array.map parse

let isEmpty (cube:cube) = 
    cube.x_min > cube.x_max || cube.y_min > cube.y_max || cube.z_min > cube.z_max

let volume (cube:cube) =
    if isEmpty cube then 0UL else
    (uint64(cube.x_max - cube.x_min + 1)) * (uint64(cube.y_max - cube.y_min + 1)) *(uint64(cube.z_max - cube.z_min + 1))

let intersection (c1:cube) (c2:cube) =
    {x_min = max c1.x_min c2.x_min; x_max = min c1.x_max c2.x_max; y_min = max c1.y_min c2.y_min; y_max = min c1.y_max c2.y_max; z_min = max c1.z_min c2.z_min; z_max = min c1.z_max c2.z_max}

let intersect (c1:cube) (c2:cube) = intersection c1 c2 |> isEmpty |> not

let part1 = 
    steps
    |> Array.map(fun t -> {t with cube =  {x_min = max t.cube.x_min -50; x_max = min t.cube.x_max 50; y_min = max t.cube.y_min -50; y_max = min t.cube.y_max 50; z_min = max t.cube.z_min -50; z_max = min t.cube.z_max 50 }})
    |> Array.filter(fun t -> t.cube |> isEmpty |> not)
    |> Array.fold(fun points t -> 
        let newPoints = 
            seq{for x in [t.cube.x_min..t.cube.x_max] do
                for y in [t.cube.y_min..t.cube.y_max] do
                for z in [t.cube.z_min..t.cube.z_max] do
                yield x,y,z} |> Set
        if t.isOn then 
            points + newPoints
        else
            points - newPoints
        ) Set.empty
    |> Set.count

let part2 = 
    let cube_shadows_pair,_ = 
        steps
        |> Seq.rev
        |> Seq.fold(fun ((on_cubes, shadows):((cube* (cube list)) list) * cube list) reboot_step -> 
            if reboot_step.isOn then
                let local_shadows = shadows  |> List.map(intersection reboot_step.cube) |> List.filter(isEmpty >> not)
                (reboot_step.cube, local_shadows)::on_cubes, reboot_step.cube::shadows
            else
                on_cubes, reboot_step.cube::shadows) ([],[])

    let lit_cubes = 
        cube_shadows_pair
        |> List.collect(fun (on_cube, shadows) -> 
            let all_cubes = on_cube::shadows        
            let x_coordinates = all_cubes |> List.collect(fun x -> [x.x_max;x.x_min]) |> List.sort |> List.windowed 2
            let y_coordinates = all_cubes |> List.collect(fun x -> [x.y_max;x.y_min]) |> List.sort |> List.windowed 2
            let z_coordinates = all_cubes |> List.collect(fun x -> [x.z_max;x.z_min]) |> List.sort |> List.windowed 2

            seq{for x in x_coordinates do
                for y in y_coordinates do
                for z in z_coordinates do
                //interior
                yield {x_min = x.Head+1; x_max = x.Tail.Head-1; y_min = y.Head+1; y_max = y.Tail.Head-1; z_min = z.Head+1; z_max = z.Tail.Head-1}
                //corners
                yield {x_min = x.Head; x_max = x.Head; y_min = y.Head; y_max = y.Head; z_min = z.Head; z_max = z.Head}
                yield {x_min = x.Head; x_max = x.Head; y_min = y.Head; y_max = y.Head; z_min = z.Tail.Head; z_max = z.Tail.Head}
                yield {x_min = x.Head; x_max = x.Head; y_min = y.Tail.Head; y_max = y.Tail.Head; z_min = z.Head; z_max = z.Head}
                yield {x_min = x.Head; x_max = x.Head; y_min = y.Tail.Head; y_max = y.Tail.Head; z_min = z.Tail.Head; z_max = z.Tail.Head}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head; y_min = y.Head; y_max = y.Head; z_min = z.Head; z_max = z.Head}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head; y_min = y.Head; y_max = y.Head; z_min = z.Tail.Head; z_max = z.Tail.Head}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head; y_min = y.Tail.Head; y_max = y.Tail.Head; z_min = z.Head; z_max = z.Head}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head; y_min = y.Tail.Head; y_max = y.Tail.Head; z_min = z.Tail.Head; z_max = z.Tail.Head}
                //surfaces
                yield {x_min = x.Head+1;    x_max = x.Tail.Head-1; y_min = y.Head+1;    y_max = y.Tail.Head-1; z_min = z.Tail.Head; z_max = z.Tail.Head}
                yield {x_min = x.Head+1;    x_max = x.Tail.Head-1; y_min = y.Head+1;    y_max = y.Tail.Head-1; z_min = z.Head;      z_max = z.Head}
                yield {x_min = x.Head+1;    x_max = x.Tail.Head-1; y_min = y.Head;      y_max = y.Head;        z_min = z.Head+1;    z_max = z.Tail.Head-1}
                yield {x_min = x.Head+1;    x_max = x.Tail.Head-1; y_min = y.Tail.Head; y_max = y.Tail.Head;   z_min = z.Head+1;    z_max = z.Tail.Head-1}
                yield {x_min = x.Head;      x_max = x.Head;        y_min = y.Head+1;    y_max = y.Tail.Head-1; z_min = z.Head+1;    z_max = z.Tail.Head-1}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head;   y_min = y.Head+1;    y_max = y.Tail.Head-1; z_min = z.Head+1;    z_max = z.Tail.Head-1}
                //edges
                yield {x_min = x.Head; x_max = x.Head; y_min = y.Head; y_max = y.Head; z_min = z.Head+1; z_max = z.Tail.Head-1}
                yield {x_min = x.Head; x_max = x.Head; y_min = y.Tail.Head; y_max = y.Tail.Head; z_min = z.Head+1; z_max = z.Tail.Head-1}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head; y_min = y.Head; y_max = y.Head; z_min = z.Head+1; z_max = z.Tail.Head-1}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head; y_min = y.Tail.Head; y_max = y.Tail.Head; z_min = z.Head+1; z_max = z.Tail.Head-1}

                yield {x_min = x.Head; x_max = x.Head; y_min = y.Head+1; y_max = y.Tail.Head-1; z_min = z.Head; z_max = z.Head}
                yield {x_min = x.Head; x_max = x.Head; y_min = y.Head+1; y_max = y.Tail.Head-1; z_min = z.Tail.Head; z_max = z.Tail.Head}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head; y_min = y.Head+1; y_max = y.Tail.Head-1; z_min = z.Head; z_max = z.Head}
                yield {x_min = x.Tail.Head; x_max = x.Tail.Head; y_min = y.Head+1; y_max = y.Tail.Head-1; z_min = z.Tail.Head; z_max = z.Tail.Head}

                yield {x_min = x.Head+1; x_max = x.Tail.Head-1; y_min = y.Head; y_max = y.Head; z_min = z.Head; z_max = z.Head}
                yield {x_min = x.Head+1; x_max = x.Tail.Head-1; y_min = y.Head; y_max = y.Head; z_min = z.Tail.Head; z_max = z.Tail.Head}
                yield {x_min = x.Head+1; x_max = x.Tail.Head-1; y_min = y.Tail.Head; y_max = y.Tail.Head; z_min = z.Head; z_max = z.Head}
                yield {x_min = x.Head+1; x_max = x.Tail.Head-1; y_min = y.Tail.Head; y_max = y.Tail.Head; z_min = z.Tail.Head; z_max = z.Tail.Head}
                }
            |> Seq.filter(isEmpty >> not)
            |> Seq.filter(fun c -> shadows |> List.exists(intersect c) |> not)
            |> Seq.distinct
            |> Seq.toList)
        
    lit_cubes |> List.sumBy volume