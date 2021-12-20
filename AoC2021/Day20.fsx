let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day20.txt") 

let algoritm = input.[0]
type image = {minX:int;maxX:int;minY:int;maxY:int; outer_pixels_on:bool; inner_pixels: Set<int*int>}

let initial_pixels = 
    input
    |> Array.skip 2
    |> Array.mapi(fun i line -> line.ToCharArray() |> Array.mapi(fun j p -> if p = '#' then Some (j,i) else None))
    |> Array.collect id
    |> Array.choose id
    |> Set

let initial_image =
    let minX = initial_pixels |> Seq.map fst |> Seq.min
    let maxX = initial_pixels |> Seq.map fst |> Seq.max
    let minY = initial_pixels |> Seq.map snd |> Seq.min
    let maxY = initial_pixels |> Seq.map snd |> Seq.max
    {minX = minX; maxX = maxX; minY = minY; maxY = maxY; outer_pixels_on = false; inner_pixels = initial_pixels} 

let index (i,j) (pixels:image) = 
    seq{for dj in [-1;0;1] do
        for di in [-1;0;1] do
        if pixels.inner_pixels.Contains (i+di, j+dj) || (pixels.outer_pixels_on && (i+di > pixels.maxX || i+di < pixels.minX || j+dj > pixels.maxY || j+dj < pixels.minY)) then
            yield "1" 
        else 
            yield "0"} 
    |> String.concat ""
    |> fun s -> System.Convert.ToInt32(s,2)

let enhance (pixels:image) =
    let new_Inner_pixels = 
        seq{for x in [pixels.minX-1 ..pixels.maxX+1] do
            for y in [pixels.minY-1 ..pixels.maxY+1] do
            let idx = index (x,y) pixels
            if algoritm.[idx] = '#' then yield (x,y)}
        |> Set
    {minX = pixels.minX-1; maxX = pixels.maxX+1; minY = pixels.minY-1;maxY = pixels.maxY+1; outer_pixels_on = not pixels.outer_pixels_on; inner_pixels = new_Inner_pixels}

let process_image iterations = 
    let enhancing = List.replicate iterations enhance |> List.reduce (>>)
    let enhanced_image = enhancing initial_image
    enhanced_image.inner_pixels.Count

let part1 = process_image 2
let part2 = process_image 50