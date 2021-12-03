let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day03.txt")

let convertBinaryString str = System.Convert.ToInt32(str, 2)

let part1 = 
    let bitCount = 
        input
        |> Seq.collect(fun binary -> binary.ToCharArray() |> Seq.indexed)
        |> Seq.groupBy fst
        |> Seq.map(fun (position, x) -> position, x |> Seq.countBy snd |> Map)
        |> Map

    let getBits criteriaFunc =
        bitCount
        |> Map.map(fun pos  bits -> 
            let v = criteriaFunc bits.['0'] bits.['1']
            let bit = bits |> Map.toSeq |> Seq.filter(fun (a,b) -> b = v) |> Seq.sortDescending |> Seq.head
            fst bit
            )
        |> Map.toSeq
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Seq.map string
        |> String.concat ""
    
    let gamma   = getBits max |> convertBinaryString
    let epsilon = getBits min |> convertBinaryString
    gamma*epsilon

let part2 = 
    let filterCandidateNumbers criteriaFunc position (binaryNumbers:Set<string>) =
        let bitCount = 
            binaryNumbers
            |> Seq.countBy(fun b -> b.[position])
            |> Map
        let digit = 
            if bitCount.ContainsKey '1' |> not then '0' else
            if bitCount.ContainsKey '0' |> not then '1' else
            if bitCount.['0'] = bitCount.['1'] then  
                match criteriaFunc 0 1 with 
                | 0 -> '0'
                | 1 -> '1'
                | _ -> failwith "Unexpected digit"
            else 
                let targetCount = criteriaFunc bitCount.['0'] bitCount.['1']
                bitCount |> Map.toSeq |> Seq.filter(fun (bit, count) -> count = targetCount) |> Seq.head |> fst
        binaryNumbers |> Set.filter(fun binary -> binary.[position] = digit)
    
    let rec findNumber criteriaFunc position binaryNumbers = 
        let filteredNumbers = filterCandidateNumbers criteriaFunc position binaryNumbers
        if filteredNumbers.Count = 1 then filteredNumbers.MaximumElement else
            findNumber criteriaFunc (position+1) filteredNumbers
        
    let oxygen_generator = findNumber max 0 (Set input) |> convertBinaryString
    let co2_scrubber     = findNumber min 0 (Set input) |> convertBinaryString
    oxygen_generator * co2_scrubber