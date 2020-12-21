let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day21.txt")

type food = 
    {
        Allergens : Set<string>
        Ingredients : Set<string>
    }

let parse (line:string)=
    let split = System.Text.RegularExpressions.Regex.Split(line,"\(contains? ")
    let fields = split.[0].Trim().Split(' ')
    let allergens = split.[1].Replace(")","").Replace(" ","").Split(',')
    { Allergens = Set allergens; Ingredients = fields |> Set}

let foods = 
    input
    |> Seq.map parse
    |> Set

let reduceAllergen (possibleAllergens: Map<string, Set<string> list>) =
    let matchedAllergens = 
        possibleAllergens 
        |> Map.map(fun k v -> Set.intersectMany v) 
        |> Map.filter(fun k v -> v.Count = 1)
    let newPossibleAllergens = 
        let matchedFoods = matchedAllergens |> Map.toSeq  |> Seq.map snd |> Set.unionMany
        possibleAllergens
        |> Map.map(fun allergen foods -> 
            if matchedAllergens.ContainsKey allergen then [Set.intersectMany foods] else
            foods |> List.map(fun food -> Set.difference food matchedFoods) |> List.filter(fun food -> food.IsEmpty |> not))
    if newPossibleAllergens = possibleAllergens then None else
    Some(newPossibleAllergens, newPossibleAllergens)

let allergens = 
    foods
    |> Seq.collect(fun food -> food.Allergens |> Seq.map(fun allergen -> allergen , food.Ingredients) )
    |> Seq.groupBy fst
    |> Seq.map( fun (a,b) -> a, b |> Seq.map snd |> Seq.toList)
    |> Map

let reducedAllergens = Seq.unfold reduceAllergen allergens|> Seq.last

let part1 = 
    let allIngredients = 
        foods |> Seq.map(fun food -> food.Ingredients) |> Set.unionMany
    
    let possiblyContainsAllergens = 
        reducedAllergens
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.map Set.unionMany
        |> Set.unionMany
    let noAllergens = Set.difference allIngredients possiblyContainsAllergens
    foods
    |> Seq.sumBy(fun food -> (Set.intersect food.Ingredients noAllergens).Count)

let part2 = 
    reducedAllergens
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.toList
    |> Seq.map snd
    |> Seq.filter(fun l -> l.Length = 1)
    |> Seq.map(fun l -> l.Head)
    |> Seq.filter(fun l -> l.Count = 1)
    |> Seq.map Seq.exactlyOne
    |> String.concat ","