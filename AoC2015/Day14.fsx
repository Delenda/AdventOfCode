let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day14.txt")

type reindeer = {Name: string; Speed: int; Duration:int; Rest:int}

let getReindeer (str:string)=
    let s = str.Split(' ')
    {Name = s.[0]; Speed = s.[3] |> int; Duration = s.[6] |> int; Rest = s.[13] |> int}

let reindeer = input |> Array.map getReindeer

let distanceTravelled (time : int) (deer : reindeer) =
    let cycleLength = deer.Duration + deer.Rest
    let cycles = time / cycleLength
    let residualTravelTime = min deer.Duration (time % cycleLength)
    cycles * deer.Speed * deer.Duration + residualTravelTime * deer.Speed

let question1 = 
    reindeer 
    |> Array.map (distanceTravelled 2503)  
    |> Array.max

let step leaders sekund =
    let inFront = 
        reindeer
        |> Array.groupBy(distanceTravelled sekund)
        |> Array.maxBy fst
        |> snd
        |> List.ofArray
        |> List.map (fun x -> x.Name)
    inFront @ leaders

let question2 = 
    [1..2503]
    |> List.fold step []
    |> List.groupBy id
    |> List.maxBy(snd >> List.length) 
    |> snd
    |> List.length