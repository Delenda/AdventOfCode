let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day12.txt") |> Array.head

let question1 = 
    let cFilter c = 
        match c with
        | '-' -> c
        | '0'| '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> c
        | _ -> ';'

    let s = input.ToCharArray() |> Array.map (cFilter >> string) |> String.concat ""
    s.Split(';') |> Array.filter(fun s -> s.Length > 0) |> Array.map int |> Array.sum

type json = 
    | Object of Map<string, json>
    | List of json list
    | String of string
    | Int of int
    member this.Value = 
        match this with
        | Object m -> 
            match m |> Seq.exists(fun x -> match x.Value with | String "red" -> true | _ -> false) with
            | true -> 0
            | false -> m |> Map.map(fun k t -> t.Value) |> Seq.sumBy(fun x -> x.Value)
        | List l -> l |> List.sumBy(fun j -> j.Value)
        | String _ -> 0
        | Int x -> x

type Parser<'a> = char list -> ('a*char list) option

let map f p =
    p >> Option.map (fun (x, rest) -> (f x), rest)

let appendMap x xs = 
    match x with
    | Object m -> m |> Map.fold (fun (s: Map<string,json>) k t -> s.Add(k,t)) xs
    | _ -> xs

let cons x xs = x::xs

let rec parseObject: json Parser =
  let rec parseInner = function
    | '}'::rest -> Some (Map.empty, rest)
    | ','::text
    | text ->
        match parseJson text with
          | Some (obj, rest) -> map (appendMap obj) parseInner rest
          | None -> None

  function
  | '{'::inner -> map Object parseInner inner
  | _ -> None

and parseList: json Parser =
  let rec parseInner = function
    | ']'::rest -> Some ([], rest)
    | _::rest -> 
          match parseJson rest with
          | Some (obj, rest) -> map (cons obj) parseInner rest
          | None -> None
  function
  | '['::inner -> map json.List parseInner inner
  | _ -> None

and parseJson: json Parser = function
  | '{'::inner -> parseObject ('{'::inner)
  | '['::inner -> parseList ('['::inner)
  | '"'::x::'"'::':'::'='::rest -> 
  | _ -> None