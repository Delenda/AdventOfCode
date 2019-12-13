let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day12.txt")

type vector = {x:int; y:int; z:int; }
type phase = {position:vector; velocity:vector}

let addVector v1 v2 = { x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z}

let addPhase moon1 moon2 = 
    { 
        position = addVector moon1.position moon2.position
        velocity = addVector moon1.velocity moon2.velocity
    }

let zero = {x=0;y=0;z=0}

let energy vect = abs(vect.x) + abs(vect.y) + abs(vect.z)
let totalEnergy phs = (energy phs.position)*(energy phs.velocity)

let parseMoon (str:string) = 
    let m = System.Text.RegularExpressions.Regex("<x=\s*(-?\d+), y=\s*(-?\d+), z=\s*(-?\d+)>").Match str
    let x = m.Groups.[1].Value |> int
    let y = m.Groups.[2].Value |> int
    let z = m.Groups.[3].Value |> int
    { position = {x = x; y = y; z= z}; velocity = zero}
    
let moons = input |> Array.map parseMoon

let testMoons1 :phase array= 
    [|
        "<x=-1, y=  0, z= 2>"
        "<x= 2, y=-10, z=-7>"
        "<x= 4, y= -8, z= 8>"
        "<x= 3, y=  5, z=-1>"
    |]
    |> Array.map parseMoon

let testMoons2 = 
    [|
        "<x=-8, y=-10, z=0>"
        "<x=5, y=5, z=10>"
        "<x=2, y=-7, z=3>"
        "<x=9, y=-8, z=-3>"    
    |] |> Array.map parseMoon

let gravityDelta moon1 moon2 = 
    let delta_vx = if moon1.position.x = moon2.position.x then 0 else if moon1.position.x < moon2.position.x then 1 else -1
    let delta_vy = if moon1.position.y = moon2.position.y then 0 else if moon1.position.y < moon2.position.y then 1 else -1
    let delta_vz = if moon1.position.z = moon2.position.z then 0 else if moon1.position.z < moon2.position.z then 1 else -1
    {position = zero; velocity = {x=delta_vx; y=delta_vy;z=delta_vz}}

let accelerate (moons: phase array) =
    moons
    |> Array.mapi(fun i m1-> 
        let delta = 
            moons 
            |> Array.mapi(fun j m2-> 
                if i = j then {position = zero; velocity = zero}
                else gravityDelta m1 m2)
            |> Array.reduce addPhase
        addPhase m1 delta)

let move moon = {moon with position = addVector moon.position moon.velocity}

let stepMoons (moons:phase array) =
    let newMoons = 
        moons
        |> accelerate
        |> Array.map move
    Some(newMoons,newMoons)

let finalEnergy n m = 
    Seq.unfold stepMoons m
    |> Seq.take n
    |> Seq.last
    |> Array.sumBy totalEnergy

let test1 = finalEnergy   10 testMoons1 = 179
let test2 = finalEnergy  100 testMoons2 = 1940
let part1 = finalEnergy 1000 moons

let firstRepeat picker moons= 
    let fstElement = moons |> Array.map picker
    Seq.unfold stepMoons moons
    |> Seq.takeWhile(fun m -> m |> Array.map picker <> fstElement)
    |> Seq.length
    |> (+) 1

type oneDimension = {pos:int; vel:int} 
let projectToX = fun v -> {pos = v.position.x; vel = v.velocity.x}
let projectToY = fun v -> {pos = v.position.y; vel = v.velocity.y}
let projectToZ = fun v -> {pos = v.position.z; vel = v.velocity.z}

let rec gcd a b = 
    let x = max (abs a) (abs b)
    let y = min (abs a) (abs b)
    let r = x%y
    if r = 0L then y else gcd y r

let lcm a b = a*b/(gcd a b)

// stepMoons is injective; therefore the phase configuration at step 0 is the first to reappear
let cyclelength moons = 
    let lx = firstRepeat projectToX moons |> int64
    let ly = firstRepeat projectToY moons |> int64
    let lz = firstRepeat projectToZ moons |> int64
    [lx;ly;lz] |> List.reduce lcm

let test3 = cyclelength testMoons1 = 2772L
let test4 = cyclelength testMoons2 = 4686774924L
let part2 = cyclelength moons