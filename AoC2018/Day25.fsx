let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\Day25.txt")

type point = 
    {
        x : int
        y : int
        z : int
        w : int
    }

let parse (s:string) = 
    let f = s.Split(',')
    {
        x = f.[0] |> int
        y = f.[1] |> int
        z = f.[2] |> int
        w = f.[3] |> int
    }

let dist a b = 
    abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.w - b.w)

let findLinks (pa : point array) = 
    pa
    |> Array.map(fun p -> (p, pa |> Array.filter(fun q -> p<> q && dist p q < 4) ) )
    |> Map

let countComponents (pa : point array)=
    let links = findLinks pa
    let findComponent ((seen,frontier) : Set<point>*(point list)) = 
        if frontier.IsEmpty then
            None
        else
            let node = frontier.Head
            let nbs = Set.difference (links.[node] |> Set) seen
            let newSeen = seen.Add node
            let newFrontier = (nbs |> Seq.toList)@frontier.Tail
            let q = (newSeen, newFrontier)
            Some(q,q)
    let findComponents ((vertices,components) : Set<point>*(Set<point> list)) =
        if vertices.IsEmpty then
            None
        else
            let node = vertices |> Seq.head
            let connectedComponent, front = 
                Seq.unfold findComponent (Set.empty, [node])
                |> Seq.last
            let newVertices = Set.difference vertices connectedComponent
            let newComponents = connectedComponent::components
            let q = (newVertices,newComponents)
            Some(q,q)
    Seq.unfold findComponents (Set pa, [])
    |> Seq.last
    |> snd
    |> List.length

let part1 =
    let points = input |> Array.map parse
    countComponents points