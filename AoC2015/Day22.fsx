let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__+ @"\Input\Day22.txt")

let boss_hitpoints, boss_damage = 
    System.Text.RegularExpressions.Regex.Match(input, @"Hit Points: (\d+)").Groups.[1].Value |> int,
    System.Text.RegularExpressions.Regex.Match(input, @"Damage: (\d+)").Groups.[1].Value |> int

type state = 
    {
        Boss_hitpoints : int
        Hitpoints : int
        Mana_used : int
        Shield_timer : int
        Poison_timer : int
        Recharge_timer :int
        Mana : int
        Armor : int
        Initial_damage : int
    }

let initialState = 
    {
        Boss_hitpoints = boss_hitpoints
        Hitpoints =  50
        Mana_used = 0
        Shield_timer = 0
        Poison_timer = 0
        Recharge_timer = 0
        Mana = 500
        Armor = 0
        Initial_damage = 0
    }

type spell = 
    | MagicMissile 
    | Drain
    | Shield
    | Poison
    | Recharge

let spellcost = function
    | MagicMissile -> 53
    | Drain -> 73
    | Shield -> 113
    | Poison -> 173
    | Recharge -> 229

let damage = function
    | MagicMissile -> 4
    | Drain -> 2
    | Shield -> 0
    | Poison -> 0
    | Recharge -> 0

let healing = function
    | Drain -> 2
    | _ -> 0

let spellAvailable state spell = 
    if spellcost spell > state.Mana then false else
    let timer = 
        match spell with
        | MagicMissile -> 0
        | Drain  -> 0
        | Shield -> state.Shield_timer
        | Poison -> state.Poison_timer
        | Recharge -> state.Recharge_timer
    timer = 0

let startOfTurn state = 
    let recharge_mana = if state.Recharge_timer > 0 then 101 else 0
    let poison_damage = if state.Poison_timer > 0 then 3 else 0
    let armor = if state.Shield_timer > 0 then 7 else 0
    let newRecharge_timer = state.Recharge_timer - 1 |> max 0
    let newPoison_timer = state.Poison_timer - 1 |> max 0
    let newShield_timer = state.Shield_timer - 1 |> max 0
    {state with 
        Boss_hitpoints = state.Boss_hitpoints - poison_damage
        Mana = state.Mana + recharge_mana
        Armor = armor
        Recharge_timer = newRecharge_timer
        Poison_timer  = newPoison_timer
        Shield_timer = newShield_timer}
     
let playerTurn spell state = 
    let cost = spellcost spell
    let damage_dealt = damage spell
    let healing = healing spell
    let rechargeTimer = if spell = Recharge then 5 else state.Recharge_timer
    let poisonTimer = if spell = Poison then 6 else state.Poison_timer
    let shieldTimer = if spell = Shield then 6 else state.Shield_timer
    {  state with 
        Mana = state.Mana - cost 
        Mana_used = state.Mana_used + cost
        Hitpoints = state.Hitpoints + healing 
        Boss_hitpoints = state.Boss_hitpoints - damage_dealt
        Recharge_timer = rechargeTimer
        Poison_timer = poisonTimer
        Shield_timer = shieldTimer}

let bossTurn state = 
    let newState = startOfTurn state
    if newState.Boss_hitpoints > 0 then 
        let damage_received = max (boss_damage - newState.Armor) 1
        {newState with 
            Hitpoints = newState.Hitpoints - damage_received}
    else
        newState

let spells = [MagicMissile ;Shield; Poison; Recharge; Drain]

let rec battle (runningBound:int option) state : int option = 
    if state.Mana < 0 then 
        None 
    elif runningBound.IsSome && state.Mana_used > runningBound.Value then 
        None
    elif state.Boss_hitpoints < 1 then 
        runningBound 
        |> Option.map(fun s -> min s state.Mana_used) 
        |> Option.defaultValue state.Mana_used
        |> Some
    elif state.Hitpoints <= state.Initial_damage then
        None
    else
        let stateWithEffects = {state with Hitpoints = state.Hitpoints - state.Initial_damage} |> startOfTurn
        spells 
            |> List.filter (spellAvailable stateWithEffects)
            |> List.fold(fun bound spell -> 
                let newState = stateWithEffects |> playerTurn spell |> bossTurn
                let newBound = battle bound newState
                if newBound.IsNone then bound
                else if bound.IsSome then min newBound.Value bound.Value |> Some
                else newBound
                ) runningBound

let part1 = battle None initialState
let part2 = battle None {initialState with Initial_damage = 1}