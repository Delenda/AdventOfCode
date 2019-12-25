let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ +   @"\Input\Day21.txt")

type item ={Cost:int; Damage:int; Armor:int}
    
let additems i1 i2 = 
    { Cost = i1.Cost + i2.Cost; Damage = i1.Damage + i2.Damage; Armor = i1.Armor + i2.Armor}

let dagger = {Cost = 8; Damage = 4; Armor = 0}
let shortsword  = {Cost = 10; Damage = 5; Armor = 0}
let warhammer = {Cost = 25; Damage = 6; Armor = 0}
let longsword = {Cost = 40; Damage = 7; Armor = 0}
let greataxe = {Cost = 74; Damage = 8; Armor = 0}

let weapons = [dagger;shortsword;warhammer;longsword;greataxe]

let leather = {Cost = 13; Damage = 0; Armor = 1}
let chainmail = {Cost = 31; Damage = 0; Armor = 2}
let splintmail = { Cost = 53; Damage = 0; Armor = 3}
let bandedmail = {Cost = 75; Damage = 0; Armor = 4}
let platemail = {Cost = 102; Damage = 0; Armor = 5}
let noArmor = {Cost = 0; Damage = 0; Armor = 0}

let armors = [leather;chainmail;splintmail;bandedmail;platemail;noArmor]

let damage1 = {Cost = 25; Damage = 1; Armor = 0}
let damage2 = {Cost = 50; Damage = 2; Armor = 0}
let damage3 = {Cost = 100; Damage = 3; Armor = 0}
let defense1 = {Cost = 20; Damage = 0; Armor = 1}
let defense2 = {Cost = 40; Damage = 0; Armor = 2}
let defense3 = {Cost = 80; Damage = 0; Armor = 3}
let noRing = {Cost = 0; Damage = 0; Armor = 0}

let rings = [damage1; damage2; damage3; defense1; defense2; defense3;noRing] 

type player = {HP:int; Damage:int;Armor:int}

let boss = 
    let regex = System.Text.RegularExpressions.Regex(@"(\d+)")
    {
        HP = regex.Match(input.[0]).Groups.[1].Value |> int
        Damage = regex.Match(input.[1]).Groups.[1].Value |> int
        Armor = regex.Match(input.[2]).Groups.[1].Value |> int
    }

let turnsToDefeat (attacker:player) (defender:player) = 
    let netDamage = attacker.Damage - defender.Armor
    if netDamage <= 0 then System.Int32.MaxValue else
    if defender.HP%netDamage = 0 then 
        defender.HP/netDamage 
    else
        1 + defender.HP/netDamage

let attackerWins attacker defender = 
    (turnsToDefeat attacker defender) <= (turnsToDefeat defender attacker)

let possibleItems = 
    seq{for weapon in weapons do
        for armor in armors do
        for ring1 in rings do
        for ring2 in rings do
        for ring3 in rings do
        if (ring1 = noRing || (ring1 <> ring2 && ring1 <> ring3)) && (ring2 = noRing || (ring2 <> ring1 && ring2 <> ring3)) && (ring3 = noRing || (ring3 <> ring2 && ring1 <> ring3)) then 
            yield [weapon; armor;ring1; ring2; ring3] |> List.reduce additems}

let part1 = 
    possibleItems
    |> Seq.filter(fun items -> attackerWins {HP = 100; Damage = items.Damage; Armor = items.Armor} boss)
    |> Seq.minBy(fun items -> items.Cost)
    |> fun i -> i.Cost

let part2 = 
    possibleItems
    |> Seq.filter(fun items -> attackerWins {HP = 100; Damage = items.Damage; Armor = items.Armor} boss |> not)
    |> Seq.maxBy(fun items -> items.Cost)
    |> fun i -> i.Cost