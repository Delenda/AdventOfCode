let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day16.txt")

type opcode = Map<int,int> -> int -> int -> int -> Map<int,int>
    

let RegisterOpcode (combinator : int->int->int ) : opcode =
    let f=
        fun (r:Map<int,int>) a b c -> r.Remove(c).Add(c, combinator r.[a] r.[b])
    f

let ImmediateOpcode (combinator : int->int->int) :opcode  =
    let f=
        fun (r:Map<int,int>) a b c -> r.Remove(c).Add(c, combinator r.[a] b)
    f



let addr = RegisterOpcode (+)
let addi = ImmediateOpcode (+)
let mulr = RegisterOpcode (*)
let muli = ImmediateOpcode (*)
let banr = RegisterOpcode (&&&)
let bani = ImmediateOpcode (&&&)
let borr = RegisterOpcode (|||)
let bori = ImmediateOpcode (|||)
let setr = fun (r:Map<int,int>) a b c -> r.Remove(c).Add(c,r.[a])
let seti = fun (r:Map<int,int>) a b c -> r.Remove(c).Add(c,a)
let gtir = fun (r:Map<int,int>) a b c -> if a > r.[b] then r.Remove(c).Add(c,1) else r.Remove(c).Add(c,0)
let gtri = fun (r:Map<int,int>) a b c -> if r.[a] > b then r.Remove(c).Add(c,1) else r.Remove(c).Add(c,0)
let gtrr = fun (r:Map<int,int>) a b c -> if r.[a] > r.[b] then r.Remove(c).Add(c,1) else r.Remove(c).Add(c,0)
let eqir = fun (r:Map<int,int>) a b c -> if a = r.[b] then r.Remove(c).Add(c,1) else r.Remove(c).Add(c,0)
let eqri = fun (r:Map<int,int>) a b c -> if r.[a] = b then r.Remove(c).Add(c,1) else r.Remove(c).Add(c,0)
let eqrr = fun (r:Map<int,int>) a b c -> if r.[a] = r.[b] then r.Remove(c).Add(c,1) else r.Remove(c).Add(c,0)

type sample = {before : Map<int,int>; after : Map<int,int>; oc:int; a :int ; b:int; c:int}

let lastSampleLine = 
    input 
    |> Array.indexed 
    |> Array.filter(fun (x,y) -> y.Contains "After")
    |> Array.map fst
    |> Array.last
    |> (+) 2

let parseSample (sample:string array) =
    let before = sample.[0].Replace(" ","").Replace("Before:","").Replace("[","").Replace("]","").Split(',') |> Array.map int
    let instruction = sample.[1].Split(' ') |> Array.map int
    let after = sample.[2].Replace(" ","").Replace("After:","").Replace("[","").Replace("]","").Split(',') |> Array.map int
    {
        before = before |> Array.indexed |> Map
        after = after |> Array.indexed |> Map
        oc = instruction.[0]
        a =  instruction.[1]
        b =  instruction.[2]
        c =  instruction.[3]
    }

let samples = 
    input
    |> Array.take lastSampleLine
    |> Array.chunkBySize 4
    |> Array.map parseSample

let opcodes = [|addr;addi;mulr;muli;banr;bani;borr;bori;setr;seti;gtir;gtri;gtrr;eqir;eqri;eqrr|]


let checkOpcode (sample:sample) (oc:opcode) =
    oc (sample.before)  sample.a sample.b sample.c = sample.after

let opcodeIndex =
    let x  =
        samples 
        |> Array.map(fun sample -> (sample.oc, opcodes |> Array.indexed |> Array.filter(snd >> checkOpcode sample) |> Array.map fst |> Set ))
        |> Array.groupBy fst
        |> Array.map (fun (x,y) -> (x, y |> Array.map snd |> Set.intersectMany  ))

    let unfolder (a,b) =
        if b |> Array.length = 0 then
            None
        else
            let y = b |> Array.filter(fun (h,g) -> g |> Seq.length = 1) |> Array.map(fun (g,h) -> (g,h |> Seq.exactlyOne))
            let q = Array.concat[|a; y|]
            let keys = y |> Array.map fst
            let values = y |> Array.map snd |> Set
            let newB = 
                b 
                |> Array.filter(fun (g,h) -> keys |> Array.contains g |> not)
                |> Array.map(fun (g,h) -> (g, Set.difference h values))
            let w = (q,newB)
            Some(w,w)
    Seq.unfold unfolder ([||], x)
    |> Seq.last
    |> fst
    |> Map

let execute (s: Map<int,int>) (t:string) =
    let fields = t.Split(' ') |> Array.map int
    let oc = fields.[0]
    let a = fields.[1]
    let b = fields.[2]
    let c = fields.[3]
    opcodes.[opcodeIndex.[oc]] s a b c

    
let part1 = 
    samples 
    |> Array.filter(fun sample -> opcodes |> Array.filter(checkOpcode sample) |> Array.length > 2) 
    |> Array.length

let part2 = 
    input 
    |> Array.skip (lastSampleLine+2)
    |> Array.fold execute ([(0,0);(1,0);(2,0);(3,0)]|> Map)  
    |> fun r -> r.[0] 

