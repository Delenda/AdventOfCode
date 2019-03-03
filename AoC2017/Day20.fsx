let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day20.txt")

type vector = 
    {x: int; y : int; z : int}
     
type vector with static member (+) (first, second) = {x = first.x + second.x; y = first.y + second.y; z = first.z + second.z}
type vector with static member (*) (m, p) = {x = m * p.x; y = m * p.y; z = m * p.z}
        
type particle = {nr: int; p : vector; v : vector; a : vector}

let rpl (s : string) (t : string) =
    s.Replace(t, "")

let parseVector (vStr : string) = 
    let tStr = ["p"; "v"; "a"; "<"; ">"; "="] |> List.fold rpl vStr
    let a = tStr.Split(',') |> Array.map int
    {x =  a.[0]; y = a.[1]; z = a.[2] }

let parseParticle (n: int) (pStr : string) =
    let a = pStr.Replace(", ",";").Split(';') |> Array.map parseVector
    {nr = n; p = a.[0]; v = a.[1]; a = a.[2]}

let particles = input |> Array.mapi parseParticle

let question1 = 
    let maxAcc p = abs p.a.x + abs p.a.y + abs p.a.z
    particles |> Array.sortBy maxAcc |> Array.head |> fun p -> p.nr

//pos(n) = p + n*v + n * (n+1) /2 * a
//pos1(n) = pos2(n)
// <=>
// p1 + n * v1 + n * (n+1)/2 * a1 = p2 + n * v2 + n * (n+1) /2 * a2
// <=>
// 2 * (p2 - p1) = 2 * n * v1 - 2 * n * v2 + n * (n+1) * a1 - n * (n+1) * a2
// <=>
// 2 * (p2 - p1) = n * [ 2* (v1 - v2) + (n+1) * (a1 - a2)]
// I.e: n divides 2 * (p2 -p1) (assuming p2<> p1)
let question2 = 
    let factors n = seq {
        for d in 1 .. (float >> sqrt >> int) n do
        if n % d = 0 then
            yield d
            yield n / d
    }

    let move n p =
        let v = n * p.v
        let a = n * (n+1) / 2 * p.a
        {nr = p.nr; p = p.p + v + a; v = p.v + n * p.a; a = p.a}

    let collide p1 p2 =
        let delta = p1.p + (-1) * p2.p
        let nx = 2 * delta.x |> abs |> factors |> Set.ofSeq
        let ny = 2 * delta.y |> abs |> factors |> Set.ofSeq
        let nz = 2 * delta.z |> abs |> factors |> Set.ofSeq

        Set.unionMany [nx; ny; nz] |> Seq.map(fun n -> (move n p1, move n p2)) |> Seq.filter(fun (x,y) -> x.p = y.p) |> Seq.length > 0

    let pairs = 
        particles 
        |> Array.map(fun p -> particles |> Array.map(fun q -> (p,q)) |> Array.filter(fun (p,q) -> p < q) )
        |> Array.collect id 
        |> Array.distinct 

    let collisionCount = 
        pairs 
        |> Array.filter(fun (p,q) -> collide p q) 
        |> Array.collect(fun (x,y) -> [|x;y|]) 
        |> Array.distinct 
        |> Array.length

    particles.Length - collisionCount