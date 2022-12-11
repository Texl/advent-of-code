namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/10
module Day10 =

    let data = EmbeddedResource.loadText "Data/Day10.txt"
    
    type Instruction =
        | Addx of int
        | Noop
        member this.Cycles =
            match this with
            | Addx _ -> 2
            | Noop -> 1
        member this.Apply x =
            match this with
            | Addx delta -> x + delta
            | Noop -> x

    type PendingInstruction =
        { Instruction : Instruction
          RemainingCycles : int }
        static member Create instruction =
            { Instruction  = instruction
              RemainingCycles = instruction.Cycles }


    type InstructionState =
        { Pending : PendingInstruction option
          Remaining : Instruction list }
        
    type State =
        { Tick : int
          X : int
          Instructions : InstructionState }
        static member Initial initialInstructions =
            { Tick = 0
              X = 1
              Instructions =
                  { Pending = None
                    Remaining = initialInstructions } }
    
    let instructions =
        data
        |> Seq.map (function
            | Regex "addx (-{0,1}\d*)" [ Int x ] -> Addx x
            | "noop" -> Noop
            | _ -> failwith "Invalid instruction")
        |> List.ofSeq

    let states =
        let initialState = instructions |> State.Initial
        
        initialState
        |> Seq.unfold (fun s ->
            opt {
                match s.Instructions with
                | { Pending = None; Remaining = nextInstruction :: remainingInstructions } ->
                    return
                        { Tick = s.Tick + 1
                          X = s.X
                          Instructions =
                            { Pending = Some (PendingInstruction.Create nextInstruction)
                              Remaining = remainingInstructions } }
            
                | { Pending = Some pending } ->
                    match pending.RemainingCycles - 1, s.Instructions.Remaining with
                    | 0, nextInstruction :: remainingInstructions ->
                        return
                            { Tick = s.Tick + 1
                              X = pending.Instruction.Apply s.X
                              Instructions =
                                { Pending = Some (PendingInstruction.Create nextInstruction)
                                  Remaining = remainingInstructions } }
            
                    | 0, [] -> ()
            
                    | n, _ ->
                        return
                            { Tick = s.Tick + 1
                              X = s.X
                              Instructions =
                                { Pending = Some { pending with RemainingCycles = n }
                                  Remaining = s.Instructions.Remaining } }
                    
                | { Pending = None; Remaining = [] } -> ()
            }
            |> Option.map (fun s -> s, s))

    let part1 () =
        let tickToStrength =
            states
            |> Seq.map (fun s -> s.Tick, s.Tick * s.X)
            |> Map.ofSeq
    
        [20; 60; 100; 140; 180; 220]
        |> List.sumBy (flip Map.find tickToStrength)
        |> printfn "%d"

    let part2 () =
        states
        |> Seq.chunkBySize 40
        |> Seq.iter (fun row ->
            row
            |> Seq.mapi (fun colIndex state ->
                if abs (colIndex - state.X) <= 1
                then "#"
                else ".")
            |> String.concat ""
            |> printfn "%s")
