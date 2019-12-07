let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\Input\Day07.txt") 

#load "Day05b.fsx"

let maxThrust program = 
    seq{for phaseA in [0..4] do
        for phaseB in [0..4] do
        for phaseC in [0..4] do
        for phaseD in [0..4] do
        for phaseE in [0..4] do
        if [phaseA;phaseB;phaseC;phaseD;phaseE] |> List.distinct |> List.length = 5 then
            let outputA = Day05b.runProgram program [phaseA;0]
            let outputB = Day05b.runProgram program [phaseB;outputA]
            let outputC = Day05b.runProgram program [phaseC;outputB]
            let outputD = Day05b.runProgram program [phaseD;outputC]
            let outputE = Day05b.runProgram program [phaseE;outputD]
            yield outputE
    }
    |> Seq.max

let test1 = maxThrust "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" = 43210
let test2 = maxThrust "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" = 54321
let test3 = maxThrust "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" = 65210

let part1 = maxThrust input

type amplifier = 
    | AmpA
    | AmpB
    | AmpC
    | AmpD
    | AmpE

type programState = int * Map<int,int> * int list * int list
type programsState = Map<amplifier, programState> 

let initPrograms program phaseA phaseB phaseC phaseD phaseE : programsState= 
    [ 
        AmpA, (0, Day05b.createMemory program, [], [phaseA; 0])
        AmpB, (0, Day05b.createMemory program, [], [phaseB])
        AmpC, (0, Day05b.createMemory program, [], [phaseC])
        AmpD, (0, Day05b.createMemory program, [], [phaseD])
        AmpE, (0, Day05b.createMemory program, [], [phaseE])
    ] |> Map

let nextAmplifier = function
    | AmpA -> AmpB
    | AmpB -> AmpC
    | AmpC -> AmpD
    | AmpD -> AmpE
    | AmpE -> AmpA

let rec stepPrograms ((amp, state, thrust) : amplifier*programsState*int list) =
    let (initPos, initMem, _, _) as progState = state.[amp]
    if (Day05b.nextOpcode initPos initMem = Day05b.Halt) then
        None
    else
        let (pos, mem, output, inp) = Seq.unfold Day05b.programStep progState |> Seq.last
        let newAmp = nextAmplifier amp
        let (tmpPos, tmpMem, _, tmpInp) = state.[newAmp]
        let newProgsState = 
            state
                .Add(amp,    (pos,    mem,    [], inp))
                .Add(newAmp, (tmpPos, tmpMem, [], tmpInp@(output |> List.rev)))
        let newThrust = 
            if amp = AmpE then
                output@thrust
            else 
                thrust
        let newState = (newAmp, newProgsState, newThrust)
        Some (newThrust, newState)

let runPrograms program phaseA phaseB phaseC phaseD phaseE=
    let programs = initPrograms program phaseA phaseB phaseC phaseD phaseE
    let initialState= AmpA, programs, []
    let finalThrust = 
        Seq.unfold stepPrograms initialState
        |> Seq.last
        |> Seq.head
    finalThrust
 
let maxThrust2 program = 
     seq{for phaseA in [5..9] do
         for phaseB in [5..9] do
         for phaseC in [5..9] do
         for phaseD in [5..9] do
         for phaseE in [5..9] do
         if [phaseA;phaseB;phaseC;phaseD;phaseE] |> List.distinct |> List.length = 5 then
             yield runPrograms program phaseA phaseB phaseC phaseD phaseE}
     |> Seq.max

let test4 = runPrograms "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" 9 8 7 6 5 = 139629729
let test5 = runPrograms "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" 9 7 8 5 6 = 18216
let test6 = maxThrust2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" = 139629729
let test7 = maxThrust2 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" = 18216

let part2 = maxThrust2 input