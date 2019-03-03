let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day02.txt")

type box = 
    {l: int; d : int; w :int}
    member this.Area = 
        let a = this.l * this.d
        let b = this.l * this.w
        let c = this.d * this.w
        let e = min a (min b c)
        2*a + 2*b + 2*c + e
    member this.Volume =
        this.l * this.d * this.w
    member this.Perimeter = 
        let a = this.l + this.d
        let b = this.l + this.w
        let c = this.d + this.w
        let e = min a (min b c)
        2*e

let getBox (ia : int array) = {l = ia.[0]; d = ia.[1]; w = ia.[2]}

let dims = input |> Array.map(fun s -> s.Split('x') |> Array.map int) |> Array.map getBox

let question1 = 
    dims |> Array.sumBy(fun b -> b.Area)

let question2 = 
    dims |> Array.sumBy(fun b -> b.Volume + b.Perimeter)