let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day19.txt") 

type Material = | Ore | Clay | Obsidian | Geode


type blueprint = 
   {Id :int 
    RobotCost : Map<Material, Map<Material,int>>
    MaxCost : Map<Material, int>
    }

let parseFactory (line:string) = 
    let pattern = "Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
    let m = System.Text.RegularExpressions.Regex(pattern).Match line
    let getint (n:int) = m.Groups.[n].Value |> int
    let robotCost = Map [Ore, Map[Ore,getint 2]; Clay, Map [Ore, getint 3]; Obsidian, Map[Ore, getint 4; Clay, getint 5]; Geode, Map[Ore, getint 6; Obsidian, getint 7] ]
    {
        Id = getint 1
        RobotCost = robotCost
        MaxCost = robotCost |> Map.toSeq |> Seq.collect(fun (_,b) -> b |> Map.toSeq) |> Seq.groupBy fst |> Seq.map(fun (a,b) -> a, b |> Seq.map snd |> Seq.max) |> Map
    }


let blueprints = input |> Array.map parseFactory

type state = 
    {
        Time : int
        Robots : Map<Material, int>
        Materials : Map<Material,int>
    }

let nextPossibleStates maxTime (blueprint: blueprint) (state:state) = 
    let newMaterials = state.Materials |> Map.map(fun material amount -> amount + state.Robots.[material])
    let newTime = state.Time + 1
    let t = maxTime - state.Time
    let enoughOre = t*blueprint.MaxCost.[Ore] - (t-1)*state.Robots.[Ore] 
    let enoughClay = t*blueprint.MaxCost.[Clay] - (t-1)*state.Robots.[Clay]  
    let enoughObs = t*blueprint.MaxCost.[Obsidian] -  (t-1)*state.Robots.[Obsidian] 
    let enough = Map [Ore, enoughOre; Clay, enoughClay; Obsidian, enoughObs]
    seq{ 
         let makeGeodeRobot = blueprint.RobotCost.[Geode].[Ore] <= state.Materials.[Ore] && blueprint.RobotCost.[Geode].[Obsidian] <= state.Materials.[Obsidian]
         if  makeGeodeRobot then
            yield {state with Time = newTime; Robots = state.Robots.Add(Geode, state.Robots.[Geode] + 1); Materials = newMaterials.Add(Ore, newMaterials.[Ore] - blueprint.RobotCost.[Geode].[Ore]).Add(Obsidian, newMaterials.[Obsidian] - blueprint.RobotCost.[Geode].[Obsidian])}
         if blueprint.RobotCost.[Obsidian].[Ore] <= state.Materials.[Ore] && blueprint.RobotCost.[Obsidian].[Clay] <= state.Materials.[Clay] && (state.Materials.[Obsidian] < enoughObs) && state.Robots.[Obsidian] < blueprint.MaxCost.[Obsidian]   then
            yield {state with Time = newTime; Robots = state.Robots.Add(Obsidian, state.Robots.[Obsidian] + 1); Materials = newMaterials.Add(Ore, newMaterials.[Ore] - blueprint.RobotCost.[Obsidian].[Ore]).Add(Clay, newMaterials.[Clay] - blueprint.RobotCost.[Obsidian].[Clay])}
         if blueprint.RobotCost.[Clay].[Ore] <= state.Materials.[Ore] && (state.Materials.[Clay] < enoughClay) && state.Robots.[Clay] < blueprint.MaxCost.[Clay] then
            yield {state with Time = newTime; Robots = state.Robots.Add(Clay, state.Robots.[Clay] + 1); Materials = newMaterials.Add(Ore, newMaterials.[Ore] - blueprint.RobotCost.[Clay].[Ore])}
         if blueprint.RobotCost.[Ore].[Ore] <= state.Materials.[Ore] && (state.Materials.[Ore] < enoughOre) && state.Robots.[Ore] < blueprint.MaxCost.[Ore]then
            yield {state with Time = newTime; Robots = state.Robots.Add(Ore, state.Robots.[Ore] + 1); Materials = newMaterials.Add(Ore, newMaterials.[Ore] - blueprint.RobotCost.[Ore].[Ore])}
         
         yield {state with Time = newTime; Materials = newMaterials}
         
            }
    |> Seq.map(fun s ->     
                  let newM = s.Materials |> Map.map(fun k v -> if k = Geode then v else min v enough.[k])
                  {s with Materials = newM})


let solve time blueprint =                     
    let cache =  new System.Collections.Concurrent.ConcurrentDictionary<state,int option>()

    let rec workFactory maxTime (blueprint:blueprint) (bound: int option) (state:state) : int option= 
        if state.Time = maxTime then state.Materials.TryFind Geode else
        let t =maxTime - state.Time
        let triangular = (t-1)*t/2
        let maxGeode = state.Materials.[Geode] + state.Robots.[Geode]*t + triangular
        if bound.IsSome && bound.Value > maxGeode then None else

        if cache.ContainsKey state then cache.[state] else
            let v = 
                nextPossibleStates maxTime blueprint state 
                |> Seq.fold(fun s t -> 
                    let newBound = workFactory maxTime blueprint s t
                    if s.IsNone then 
                        newBound
                    else if newBound.IsNone then
                        s
                    else 
                        Some(max s.Value newBound.Value)
                    ) bound

            let k = cache.AddOrUpdate(state, v, new System.Func<state,int option,int option>(fun _ _ -> v)) 
            v
    let initialState = {Time = 0; Robots = Map [Ore,1; Clay,0; Obsidian,0; Geode,0]; Materials = Map [Ore,0; Clay,0; Obsidian,0; Geode,0]}
    
    workFactory time blueprint None initialState
let sw1 = System.Diagnostics.Stopwatch.StartNew()
let part1 = 
    blueprints 
    |> Seq.sumBy(fun bp -> 
        let sw = System.Diagnostics.Stopwatch.StartNew()
        bp.Id |> sprintf "Calculation blueprint: %d " |> System.Console.WriteLine
        let r = solve 24 bp |> Option.defaultValue 0 |> fun s -> s * bp.Id
        sw.Elapsed.TotalSeconds |> sprintf "%d %f s" r |> System.Console.WriteLine
        r)
let duration1 = sw1.Elapsed.TotalSeconds

let sw2 = System.Diagnostics.Stopwatch.StartNew()
let part2 = 
    blueprints 
    |> Array.take 3
    |> Array.map(fun bp -> 
        let sw = System.Diagnostics.Stopwatch.StartNew()
        bp.Id |> sprintf "Calculation blueprint: %d " |> System.Console.WriteLine
        let r = solve 32 bp |> Option.defaultValue 0 
        sw.Elapsed.TotalSeconds |> sprintf "%d %f s" r |> System.Console.WriteLine
        r)
    |> Seq.reduce (*)
let duration2 = sw2.Elapsed.TotalSeconds