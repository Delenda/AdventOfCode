let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day16.txt")

let rules =
    seq{for m in System.Text.RegularExpressions.Regex.Matches(input, @"([\w|\s]+): (\d+)-(\d+) or (\d+)-(\d+)\r\n") do
        let group = m.Groups.[1].Value
        let d1 = m.Groups.[2].Value |> int
        let d2 = m.Groups.[3].Value |> int
        let d3 = m.Groups.[4].Value |> int
        let d4 = m.Groups.[5].Value |> int
        yield (group, Set.union (Set [d1..d2]) (Set [d3..d4]))
        }
    |> Map

let tickets =
    seq{for m in System.Text.RegularExpressions.Regex.Matches(input, @"(\d+,)*(\d+)") do
            yield m.Groups.[0].Value.Split(',') |> Array.map int
        }
    |> Seq.filter(fun a -> a.Length > 1)
    |> Seq.toArray

let part1 =
    let allRanges =
        rules
        |> Seq.map(fun kvp -> kvp.Value)
        |> Set.unionMany
    tickets
    |> Array.collect id
    |> Array.filter(fun d -> allRanges.Contains d |> not)
    |> Array.sum

let validTickets =
    let allRanges =
        rules
        |> Seq.map(fun kvp -> kvp.Value)
        |> Set.unionMany
    tickets
    |> Array.filter(fun a -> Set.isSubset (Set a) allRanges)

let step ((foundFields, ticketValues) : Set<string*int>*Map<int,Set<int>>)=
    let remainingRules =
        rules
        |> Map.filter(fun k v -> foundFields |> Set.map fst |> Set.contains k |> not)
    let fieldPosition, fieldName =
        remainingRules
        |> Seq.collect(fun kvp->
            let rulename = kvp.Key
            let rule_range = kvp.Value
            ticketValues |> Map.map(fun fieldNumber fieldValues ->
                fieldNumber, rulename, rule_range, fieldValues) |> Seq.map(fun k -> k.Value))
        |> Seq.filter(fun (_,_,range, ticketvalues) -> Set.isSubset ticketvalues range)
        |> Seq.map(fun (fieldnumber,rulename,_,_) -> fieldnumber,rulename)
        |> Seq.groupBy fst
        |> Seq.filter(fun (a,b) -> b |> Seq.length = 1)
        |> Seq.head
        |> snd
        |> Seq.exactlyOne
    let newFoundFields = foundFields.Add(fieldName, fieldPosition)
    let newTicketValues = ticketValues |> Map.filter(fun k _ -> k <> fieldPosition)
    newFoundFields, newTicketValues

let part2 =
    let valuesByField =
        validTickets.[0]
        |> Array.mapi(fun fieldNumber _-> fieldNumber, validTickets |> Array.map(fun a -> a.[fieldNumber]) |> Set)
        |> Map
    Seq.unfold(fun (state: Set<string*int>*Map<int,Set<int>>) ->
        if (snd state).Count = 0 then None else
        let newState = step state
        Some(newState, newState)
        ) (Set.empty, valuesByField)
    |> Seq.last
    |> fst
    |> Set.filter(fun (fieldname, idx) -> fieldname.StartsWith "departure")
    |> Seq.map (fun (fieldname, idx) -> validTickets.[0].[idx] |> uint64)
    |> Seq.reduce (*)