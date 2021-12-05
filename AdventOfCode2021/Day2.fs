namespace AdventOfCode2021

// https://adventofcode.com/2021/day/2

module Day2 =

//    --- Day 2: Dive! ---
//
//    Now, you need to figure out how to pilot this thing.
//
//    It seems like the submarine can take a series of commands like forward 1, down 2, or up 3:
//
//        forward X increases the horizontal position by X units.
//        down X increases the depth by X units.
//        up X decreases the depth by X units.
//
//    Note that since you're on a submarine, down and up affect your depth, and so they have the opposite result of what you might expect.
//
//    The submarine seems to already have a planned course (your puzzle input). You should probably figure out where it's going. For example:
//
//    forward 5
//    down 5
//    forward 8
//    up 3
//    down 8
//    forward 2
//
//    Your horizontal position and depth both start at 0. The steps above would then modify them as follows:
//
//        forward 5 adds 5 to your horizontal position, a total of 5.
//        down 5 adds 5 to your depth, resulting in a value of 5.
//        forward 8 adds 8 to your horizontal position, a total of 13.
//        up 3 decreases your depth by 3, resulting in a value of 2.
//        down 8 adds 8 to your depth, resulting in a value of 10.
//        forward 2 adds 2 to your horizontal position, a total of 15.
//
//    After following these instructions, you would have a horizontal position of 15 and a depth of 10. (Multiplying these together produces 150.)
//
//    Calculate the horizontal position and depth you would have after following the planned course. What do you get if you multiply your final horizontal position by your final depth?

    let dataFilePath = "Data/Day2.data"

    type Command =
        | Forward of int // horizonal
        | Down of int // more deeps
        | Up of int // less deeps
        
    let (|Command|_|) (str : string) =
        match str with
        | "forward" -> Some Forward
        | "down" -> Some Down
        | "up" -> Some Up
        | _ -> None
          
    let (|Int|_|) (str : string) =
        match System.Int32.TryParse(str) with
        | true, v -> Some v
        | false, _ -> None

    let parseCommand (line : string) =
        match line.Split ' ' |> List.ofArray with
        | Command command :: [ Int x ] -> command x
        | _ -> failwith $"Invalid line '{line}'"
        
    let commands =
        dataFilePath
        |> Utils.loadTextResource
        |> Seq.map parseCommand

    type SubmarineState =
        { Horizontal : int
          Depth : int }
        static member Zero =
            { Horizontal = 0
              Depth = 0 }

    let applyCommand (submarineState : SubmarineState) (command : Command) =
        match command with
        | Forward x -> { submarineState with Horizontal = submarineState.Horizontal + x }
        | Down x -> { submarineState with Depth = submarineState.Depth + x }
        | Up x -> { submarineState with Depth = submarineState.Depth - x }
        
    let part1 () =
        
        let finalSubmarineState = (SubmarineState.Zero, commands) ||> Seq.fold applyCommand
        
        printfn $"%A{finalSubmarineState} - {finalSubmarineState.Horizontal * finalSubmarineState.Depth}"
