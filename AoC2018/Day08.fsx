let input = System.IO.File.ReadAllText(@"C:\Users\Lars\source\repos\AoC2018\Input\Day08.txt").Split(' ') |> Array.map int |> List.ofSeq

type node = {metadata:int list; children : node list}
let rec nodeLength n = 2 + (n.children |> List.sumBy nodeLength) + n.metadata.Length
let rec metadataSum n = (n.metadata |> List.sum ) + (n.children |> List.sumBy metadataSum)
let rec nodeValue n = 
    if n.children.IsEmpty then
        n.metadata |> List.sum
    else
        n.metadata
        |> List.filter(fun i -> i <= n.children.Length)
        |> List.map(fun i -> n.children |> List.item(i-1))
        |> List.sumBy nodeValue

let rec getTree (ip:int list) : node =
    let num_child = ip.Head
    let num_meta = ip |> List.skip 1 |> List.head
    let getChildNode s =
        let n = getTree s
        Some (n, List.skip (nodeLength n) s)
    let children = 
        Seq.unfold getChildNode (List.skip 2 ip)
        |> Seq.take num_child
        |> List.ofSeq
    let metapos = 2 + (children |> List.sumBy nodeLength)
    let metadata = ip |> List.skip metapos |> List.take num_meta
    {metadata = metadata; children = children }
    
let part1 = (getTree >> metadataSum) input
let part2 = (getTree >> nodeValue) input

//let testinput = [|2;3;0;3;10;11;12;1;1;0;1;99;2;1;1;2|]
//let rec getmetadata (ip: int array) i : int*int=
//    if i = ip.Length then (0,0)
//    else
//        let childCount = ip.[i]
//        let metaCount = ip.[i+1]
//        let unfoldChildren s =
//            let a = getmetadata ip (snd s)
//            Some ((fst s + fst a, snd a), (fst s + fst a, snd a))
//        let subMeta = 
//            if childCount = 0 then (0,i+2) else
//                Seq.unfold unfoldChildren (0,i + 2)
//                |> Seq.take childCount
//                |> Seq.last
//        let meta = [1..metaCount] |> List.sumBy(fun i -> ip.[i - 1 + (snd subMeta)])
//        let idx = (snd subMeta) + metaCount
//        (meta + fst subMeta,  idx)

//let rec getValue (ip: int array) i : int*int=
//    if i = ip.Length then (0,0)
//    else
//        let childCount = ip.[i]
//        let metaCount = ip.[i+1]
//        if childCount = 0 then
//            let meta = [1..metaCount] |> List.sumBy(fun j -> ip.[i + 1 + j])
//            let idx = i + 2 + metaCount
//            (meta,idx)
//        else
//        let unfoldChildren s =
//            let a = getValue ip (snd s)
//            Some (a,a)
//        let subValues = 
//           Seq.unfold unfoldChildren (0,i+2)
//           |> Seq.take childCount
//           |> Seq.toArray
//        let metaPos = subValues |> Array.last |> snd
//        let retval = 
//            [metaPos..metaPos+metaCount-1] 
//            |> List.map(fun i -> ip.[i]) 
//            |> List.filter(fun i -> i <= childCount) 
//            |> List.sumBy(fun i -> subValues.[i-1] |> fst)
//        let idx = metaPos + metaCount
//        (retval,  idx)
// let part1 = 
//    getmetadata input 0 |> fst

//let part2 = 
//    getValue input 0 |> fst      