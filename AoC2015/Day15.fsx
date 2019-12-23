let input = System.IO.File.ReadAllLines(@"D:\Src\Aoc2015\Input\Day15.txt")

type ingredient =
    {
        Name : string
        capacity : int
        durability : int
        flavor : int
        texture : int
        calories : int
    }
    static member (*) (a, b: ingredient) =
        {
            Name = b.Name
            capacity = a * b.capacity
            durability = a * b.durability
            flavor = a * b.flavor
            texture = a * b.texture
            calories = a * b.calories
        }    
    static member (+) (a, b) =
        {
            Name = ""
            capacity = a.capacity + b.capacity
            durability = a.durability + b.durability
            flavor = a.flavor + b.flavor
            texture = a.texture + b.texture
            calories = a.calories + b.calories
        }
    member this.Score = 
        (max 0 this.capacity) * (max 0 this.durability) * (max 0 this.flavor) * (max 0 this.texture)

let parseIngredient (str:string)=
    let name = (str.Split(':')).[0]
    let attr = (str.Split(':')).[1].Replace(", ",";").Split(';')
    {
        Name = name
        capacity = attr.[0].Replace("capacity ","") |> int
        durability  = attr.[1].Replace("durability ","") |> int
        flavor = attr.[2].Replace("flavor ","") |> int
        texture = attr.[3].Replace("texture ","") |> int
        calories = attr.[4].Replace("calories ","") |> int
    }

let ingredients = input |> Array.map parseIngredient

let combine a b c d =
    a * ingredients.[0] + b * ingredients.[1] + c * ingredients.[2] + d * ingredients.[3]

let recipies = [0..100] |> List.collect(fun a -> [0..(100-a)] |> List.collect(fun b -> [0..(100-a-b)] |> List.map(fun c -> combine a b c (100-a-b-c))))

let question1 = 
    recipies 
    |> List.map(fun i-> i.Score) 
    |> List.max

let question2 = 
    recipies 
    |> List.filter(fun i-> i.calories = 500)
    |> List.map(fun i -> i.Score) 
    |> List.max