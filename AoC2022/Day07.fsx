let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day07.txt") 

let paths,sizes= 
    input
    |> Seq.fold(fun ((path, sizes): string list * Map<string,int>) cmd -> 
        if cmd = "$ cd .." then
            path.Tail, sizes
        else if cmd.StartsWith "$ cd " then
            let newPathName = 
                let basename = (cmd.Replace("$ cd ",""))
                if path.IsEmpty then
                    basename
                else
                    basename+"."+path.Head
            newPathName::path,sizes
        else if cmd.StartsWith "dir " then
            path,sizes
        else if cmd.StartsWith "$ ls" then
            path,sizes
        else
            let size = cmd.Split(' ').[0] |> int
            let newSizes = 
                path 
                |> List.fold(fun (szs : Map<string,int>) t -> 
                let newSize = size + (szs.TryFind t |> Option.defaultValue 0)
                szs.Add(t, newSize)) sizes
            path, newSizes) ([],Map.empty)

let part1 = 
    sizes 
    |> Map.toSeq 
    |> Seq.map snd 
    |> Seq.filter(fun d -> d <= 100000) 
    |> Seq.sum

let part2 = 
    sizes 
    |> Map.filter(fun _ v -> 70000000 - sizes.["/"] + v >= 30000000 ) 
    |> Map.toSeq 
    |> Seq.map snd 
    |> Seq.min

//let parse (terminalLines: string array) = 
//    let rec innerParse (tokens:string list) = 
//        if tokens.Head.StartsWith "$ cd " then 
//            let name = tokens.Head.Split(' ').[2]
//            let files, rest = parseDirectory tokens.Tail []
//            Directory(name, files), rest
//        else if tokens.Head.StartsWith "$ ls" then
//            innerParse tokens.Tail
//        else if tokens.Head.StartsWith "dir" then
//            innerParse tokens.Tail
//        else 
//            parseFile tokens
//    and parseDirectory (tokens:string list) (files:filesystem list) = 
//        if tokens.IsEmpty then
//            List.rev files, []
//        else if tokens.Head = "$ cd .." then 
//            List.rev files, tokens.Tail 
//        else
//            let file,rest = innerParse tokens
//            parseDirectory rest (file::files)
//    and parseFile tokens = 
//        let fields = tokens.Head.Split(' ')
//        let size = fields.[0] |> int
//        let name = fields.[1]
//        File(name,size), tokens.Tail
//    terminalLines |> Seq.toList |> innerParse |> fst

type token = 
    | Cd_Root
    | Cd_Down of name:string
    | Cd_Up
    | Ls
    | File of name:string*size:int
    | Directory of name:string

let tokenize (line:string) =
    if line = "$ cd /" then Cd_Root
    else if line = "$ ls" then Ls
    else if line = "$ cd .." then Cd_Up
    else if line.StartsWith "$ cd " then
        let name = line.Split(' ').[2]
        Cd_Down name
    else if line.StartsWith "dir" then
        let name = line.Split(' ').[1]
        Directory name
    else
        let fields = line.Split(' ')
        File(fields.[1], fields.[0] |> int)

type filesystem = 
    | Directory of string*(filesystem list)
    | File of string*int

let parse (terminalLines: string array) =
    let rec innerParse (tokens: token list) =
        match tokens with
        | Cd_Root::_            -> parseDirectory "/" tokens.Tail []
        | Cd_Down name::_       -> parseDirectory name tokens.Tail [] 
        | Cd_Up::_              -> failwith "syntax error"
        | Ls::_                 -> innerParse tokens.Tail
        | token.Directory _::_  -> innerParse tokens.Tail
        | token.File _::_       -> parseFile tokens
        | []                    -> failwith "syntax error"
    and parseDirectory name (tokens: token list) (files:filesystem list) =
        match tokens with
        | []        -> Directory(name, List.rev files), []
        | Cd_Up::_  -> Directory(name, List.rev files), tokens.Tail
        | _ -> 
            let file, rest = innerParse tokens
            parseDirectory name rest (file::files)
    and parseFile tokens = 
        match tokens with
        | token.File(name,size)::_ -> File(name,size), tokens.Tail
        | _ -> failwith "syntax error"
    terminalLines |> Seq.map tokenize |> Seq.toList |> innerParse |> fst

let directory = parse input
        
let rec size = function
    | File(_,size) -> size
    | Directory(_, files) -> files |> List.sumBy size

let rec directoryBySize criteria file = 
    match file with
    | File _ -> []
    | Directory(_, files) -> 
        let thisFile = [file] |> List.filter criteria
        let subDirectories = files |> List.collect (directoryBySize criteria)
        thisFile@subDirectories

let part1_v2 = 
    directoryBySize (fun f -> size f <= 100000)  directory
    |> List.sumBy size

let part2_v2 = 
    directoryBySize (fun f -> 70000000 - (size directory) + size f  >= 30000000 )  directory
    |> List.minBy size
    |> size
    
let name = function
    |File(name,_) -> name
    |Directory(name,_) -> name

let rec print depth (directory:filesystem) =
    let margin = (String.replicate depth " ")
    match directory with
    | File(name,size) -> sprintf "%s- %s (file, size=%d)" margin name size
    | Directory(n, files) ->
        let header = sprintf "%s- %s (dir)" margin n 
        (header::(files |> List.sortBy name |> List.map (print (depth+2))))  |> String.concat "\r"
        
print 0 directory |> System.Console.WriteLine
