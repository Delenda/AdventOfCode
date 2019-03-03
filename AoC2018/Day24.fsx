let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day24.txt")
let input_snd = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\input\day24_snd.txt")

type groupType =
    | Infection
    | ImmuneSystem

type attackType =
    | Radiation
    | Slashing
    | Fire
    | Bludgeoning
    | Cold
    
type damage = 
    {
        AttackPower : int
        AttackType : attackType
    }

type group = 
    {
        grouptype : groupType
        units : int 
        hp : int
        weakness : attackType list
        immune : attackType list
        damage : damage
        initiative : int      
        ID : int
    }
    member this.EffectivePower = this.units * this.damage.AttackPower
    member this.Damage (enemy:group) =
        if enemy.immune |> List.contains this.damage.AttackType then
            0
        else if enemy.weakness |> List.contains this.damage.AttackType then
            2 * this.EffectivePower
        else 
            this.EffectivePower
    override x.ToString() = 
        sprintf "%A %d units:%d initiative:%d EffectivePower:%d hp:%d immune:%A weakness:%A AttackPower:%d AttackType:%A" x.grouptype x.ID x.units x.initiative x.EffectivePower x.hp x.immune (x.weakness) x.damage.AttackPower x.damage.AttackType
            

let attackTypeFromString (s:string) = 
    match s with
    | "radiation" -> Radiation
    | "bludgeoning" -> Bludgeoning
    | "slashing" -> Slashing
    | "fire" -> Fire
    | "cold" -> Cold
    | _ -> failwith (sprintf "Uventet attacktype string : %s" s)


let parseATArr (s:string)=
    s.Replace(" ","").Split(',') |> Array.map attackTypeFromString |> Array.toList


let parseIW (s:string) =
    let t = s.Replace("(","").Replace(")","").Replace("weak to ","W;").Replace("immune to ","I;").Split(';')
    if t.Length = 1 then
        [],[]
    else if t.Length = 2 then
        if t.[0] = "W" then
            parseATArr t.[1], []
        else if t.[0] = "I" then
            [], parseATArr t.[1]
        else
            failwith (sprintf "Uventet t90 : %s" t.[0])
    else if t.Length = 4 then
        if t.[0] = "W" then
            parseATArr t.[1], parseATArr t.[3]
        else if t.[0] = "I" then
            parseATArr t.[3], parseATArr t.[1]
        else
            failwith "Ugyldig parsning"
    else
        failwith ("Uventet længde")


let parseGroup gt (s:string)=
    let fields = s.Replace(" units each with ",":").Replace(" hit points with ", " hit points () with ").Replace(" hit points ",":").Replace(" with an attack that does ",":").Replace(" damage at initiative ",":").Split(':')
    let attackFields = (fields.[3]).Split(' ')
    let weak, immune = parseIW fields.[2]
    {
        grouptype = gt
        units = fields.[0] |> int
        hp = fields.[1] |> int
        weakness = weak
        immune = immune
        damage = {AttackPower = attackFields.[0] |> int; AttackType = attackTypeFromString attackFields.[1]}
        initiative = fields.[4] |> int
        ID = -1
    }

let getCombatants ip =
    let immIdx = ip |> Array.findIndex(fun (s :string) -> s.Contains("Infection:"))

    let is = ip |> Array.take (immIdx - 1) |> Array.tail |> Array.map (parseGroup groupType.ImmuneSystem) |> Array.mapi(fun i x -> {x with ID = i+1})
    let inf= ip |> Array.skip (immIdx + 1 ) |> Array.map (parseGroup groupType.Infection) |> Array.mapi(fun i x -> {x with ID = i+1})

    Array.concat [|is;inf|]

let targetSelection (friends:(int*group) array) (foes:(int*group) array) =
    friends 
    |> Array.sortByDescending(fun (i,g) -> (g.EffectivePower, g.initiative))
    |> Array.fold (fun (h,s) (idx,t) -> 
        if s |> Array.filter(fun (i,foe) -> t.Damage foe > 0) |> Array.isEmpty then
            //System.Console.WriteLine(sprintf "%A %d vælger intet target" (t.grouptype) (t.ID) )
            ((idx,-1)::h,s)
        else
            //s |> Array.iter(fun x -> System.Console.WriteLine (sprintf "%A group %d would deal defending group %d %d damage" (t.grouptype) (t.ID) ((snd x).ID) (t.Damage (snd x)) ))
            let r = s |> Array.sortByDescending(fun (i,foe) -> (t.Damage foe, foe.EffectivePower, foe.initiative))
            let rv = 
                if r |> Array.isEmpty then
                    -1
                else
                    r |> Array.head |> fst
            let q = r |> Array.filter(fun (i,foe) -> i <> rv)
            ((idx,rv)::h, q)) ([], foes)
    |> fst

let fight (combatants: group array) =
    let idxComb = combatants |> Array.sortByDescending(fun g -> g.initiative) |> Array.indexed
    let idxInfection = idxComb |> Array.filter(fun (i,g) -> g.grouptype = Infection)
    let idxImmSystem = idxComb |> Array.filter(fun (i,g) -> g.grouptype = ImmuneSystem)

//    System.Console.WriteLine ""
//    System.Console.WriteLine "Immune System:"
//    
//    idxImmSystem |> Array.iter(fun (i,g) -> System.Console.WriteLine g  )
//    System.Console.WriteLine "Infection:"
//    idxInfection |> Array.iter(fun (i,g) -> System.Console.WriteLine g  )


    let infTargets = targetSelection idxInfection idxImmSystem
    let immTargets = targetSelection idxImmSystem idxInfection

    let targets = infTargets@immTargets |> Map

    let combatantMap = idxComb |> Map
    idxComb
    |> Array.map fst
    |> Array.fold (fun (s:Map<int,group>) t -> 
        let fighter =  s.[t]
        if fighter.EffectivePower = 0 then
            //System.Console.WriteLine(sprintf "%A %d er død. Angriber ikke" fighter.grouptype fighter.ID)
            s
        else if targets.[t] = -1 then
            //System.Console.WriteLine(sprintf "%A %d fandt ikke target. Angriber ikke" fighter.grouptype fighter.ID)
            s
        else
            let idxFoe = targets.[t]
            let foe = s.[idxFoe]
            //System.Console.WriteLine(sprintf "%A %d angriber %A %d" fighter.grouptype fighter.ID)
            let damage = fighter.Damage foe
            let unitsLost = min (damage / foe.hp) foe.units
            //System.Console.WriteLine(sprintf "%A %d tildeles %d skade. Mister %d enheder." foe.grouptype foe.ID damage unitsLost)
            //System.Console.WriteLine (sprintf "%A group %d attacks defending group %d, killing  %d units" fighter.grouptype fighter.ID foe.ID unitsLost)
            let newFoe = {foe with units = (foe.units - unitsLost) |> max 0}
            s.Remove(idxFoe).Add(idxFoe,newFoe)) combatantMap
    |> Seq.toArray
    |> Array.map (fun k -> k.Value)
    |> Array.filter(fun g -> g.units > 0)

let test = 
    [|
        "Immune System:"
        "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
        "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"
        ""
        "Infection:"
        "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
        "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"|]


let findCount combs = 
    Seq.unfold(fun s ->
                let q = fight s
                Some(q,q)  
                ) combs
    |> Seq.map(fun s -> s |> Array.groupBy(fun g -> g.grouptype) |> Array.map(fun (a,b) -> (a, b |> Array.sumBy(fun h -> h.units ))))
    |> Seq.skipWhile(fun s -> s.Length > 1)
    |> Seq.head
    |> Array.head
    |> snd

let boostImm (combatants:group array) boost = 
    combatants |> Array.map(fun g -> if g.grouptype = Infection then g else {g with damage = {g.damage with AttackPower = g.damage.AttackPower + boost}})

let findWinner (combatants: group array) =
    let sq =  
        Seq.unfold(fun s ->
                    let q = fight s
                    if q |> Array.sumBy(fun g -> g.units) = (s |> Array.sumBy(fun g -> g.units))  then
                        None
                    else
                        Some(q,q)  
                    ) combatants
        |> Seq.map(fun s -> s |> Array.groupBy(fun g -> g.grouptype) )
        |> Seq.skipWhile(fun s -> s.Length > 1)
        
    if sq |> Seq.isEmpty then
        None
    else 
        sq
        |> Seq.head
        |> Array.head
        |> fst
        |> Some

let f n = Array.replicate n fight |> Array.reduce(>>)

let part1 = 
    let combs = getCombatants input
    findCount combs

let part2 = 
    let c = getCombatants input

    let boost = 
        Seq.initInfinite id
        |> Seq.map(fun i -> (i,findWinner (boostImm c i)))
        |> Seq.filter(fun (a,b) -> b.IsSome && b.Value = ImmuneSystem)
        |> Seq.head
        |> fst

    findCount (boostImm c boost)
