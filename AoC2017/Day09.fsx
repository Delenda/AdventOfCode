let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + @"\Input\Day09.txt")
type State = 
    {
        Garbage         : bool
        GroupLevel      : int
        IgnoreNext      : bool
        Score           : int
        GarbageCount    : int
    }
let scan state char =
    match state.IgnoreNext, state.Garbage, char with
    | true,    _,    _  -> {state with IgnoreNext = false}
    | _,    true,   '!' -> {state with IgnoreNext = true}
    | _,    true,   '>' -> {state with Garbage = false}
    | _,    true,    _  -> {state with GarbageCount = state.GarbageCount + 1}
    | _,       _,   '<' -> {state with Garbage = true }
    | _,       _,   '{' -> {state with GroupLevel = state.GroupLevel + 1}
    | _,       _,   '}' -> {state with GroupLevel = state.GroupLevel - 1; Score = state.Score + state.GroupLevel}
    | _                 ->  state
let stream = input|> Array.map(fun s -> s.ToCharArray()) |> Array.exactlyOne
stream |> Array.fold scan {Garbage = false; GroupLevel = 0; IgnoreNext = false; Score = 0; GarbageCount = 0}