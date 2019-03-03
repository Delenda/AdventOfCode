let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\Day21.txt")

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

type command = 
    |Addr|Addi|Mulr|Muli|Banr|Bani|Borr|Bori|Setr|Seti|Gtir|Gtri|Gtrr|Eqir|Eqri|Eqrr
    static member fromString s = 
        match s with
        | "addr" -> Addr
        | "addi" -> Addi
        | "mulr" -> Mulr
        | "muli" -> Muli
        | "banr" -> Banr
        | "bani" -> Bani
        | "borr" -> Borr
        | "bori" -> Bori
        | "setr" -> Setr
        | "seti" -> Seti
        | "gtir" -> Gtir
        | "gtri" -> Gtri
        | "gtrr" -> Gtrr
        | "eqir" -> Eqir
        | "eqri" -> Eqri
        | "eqrr" -> Eqrr
        | _ -> failwith "Uventet instruktion"


type argumenter = {A:int;B:int;C:int}

type linje = 
    {Kommando:command; Argumenter:argumenter}
    member this.Apply r =
        match this.Kommando with
        | Addr -> addr r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Addi -> addi r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Mulr -> mulr r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Muli -> muli r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Banr -> banr r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Bani -> bani r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Borr -> borr r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Bori -> bori r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Setr -> setr r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Seti -> seti r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Gtir -> gtir r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Gtri -> gtri r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Gtrr -> gtrr r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Eqir -> eqir r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Eqri -> eqri r this.Argumenter.A this.Argumenter.B this.Argumenter.C
        | Eqrr -> eqrr r this.Argumenter.A this.Argumenter.B this.Argumenter.C

let parse (s:string) = 
        let fields = s.Split(' ')
        {
          Kommando = fields.[0]  |> command.fromString
          Argumenter = {A = fields.[1] |> int; B = fields.[2] |> int; C = fields.[3] |> int}
        }

let commands = 
    input
    |> Array.tail 
    |> Array.map parse

let runProgram pointerReg instructions =
    let upper = instructions |> Array.length
    let register = Array.replicate 6 0 |> Array.indexed |> Map.ofArray
    let unfolder ((r,seen,last): Map<int,int>*Set<int>*int) =
        let pointer  = r.[pointerReg]
        let newSeen, newLast = 
            if pointer = 28 then
                seen.Add(r.[3]), r.[3]
            else seen,last
        if pointer = 28 && seen.Contains(r.[3]) then
            None
        else
            let newP, newR = 
                if pointer = 17 then
                    let register2 = r.[2]
                    8,r.Remove(2).Add(2,register2/256)
                else
                    let comm : linje = instructions.[pointer]
                    let newR : Map<int,int>= comm.Apply r
                    let newP = newR.[pointerReg] + 1
                    newP,newR
            let finalR = (newR.Remove pointerReg).Add(pointerReg, newP)
            Some(newLast,(finalR,newSeen,newLast))
    Seq.unfold unfolder (register,Set.empty, -1)

let part1 = 
    runProgram 4 commands
    |> Seq.skipWhile(fun t -> t<0)
    |> Seq.head

let part2 = 
    runProgram 4 commands
    |> Seq.last