namespace AdventOfCode2022

open System
open AdventOfCode.Common

/// https://adventofcode.com/2022/day/23
module Day23 =
    let data = EmbeddedResource.loadText "Data/Day23.txt"

    let parseElves () =
        data
        |> Seq.collecti (fun r row ->
            row
            |> Seq.mapi (fun c ->
                function
                | '.' -> None
                | '#' -> Some { R = r; C = c } 
                | _ -> raise invalidInput))
        |> Seq.choose id
        |> Set.ofSeq
        
    [<Flags>]
    type Direction =
        | None = 0
        | E = 1
        | SE = 2
        | S = 4
        | SW = 8
        | W = 16
        | NW = 32
        | N = 64
        | NE = 128
        
        | PN = 224
        | PS = 14
        | PW = 56
        | PE = 131

    type ProposedDirection = PN | PS | PW | PE
        
    let proposedDirectionStream : ReadPoint<ProposedDirection list> = 
        (fun _ -> [ PN; PS; PW; PE ])
        |> Seq.initInfinite
        |> Seq.collect id
        |> Seq.tails
        |> Seq.map (Seq.truncate 4 >> List.ofSeq)
        |> ReadPoint.ofSeq

    let rec doRound (elves : Set<Vector2>) (dirStream : ReadPoint<ProposedDirection list>) (roundN : int) (stopAtRound : int option) =
        
        let rec consider position proposedDirs =
            let adjacencyFlags =
                [ Direction.N,  position + { R = -1; C =  0 }
                  Direction.NE, position + { R = -1; C =  1 }
                  Direction.E,  position + { R =  0; C =  1 }
                  Direction.SE, position + { R =  1; C =  1 }
                  Direction.S,  position + { R =  1; C =  0 }
                  Direction.SW, position + { R =  1; C = -1 }
                  Direction.W,  position + { R =  0; C = -1 }
                  Direction.NW, position + { R = -1; C = -1 } ]
                |> List.map (fun (flag, position) ->
                    if elves.Contains position
                    then flag
                    else Direction.None)
                |> List.reduce (|||)

            let rec tryStep =
                function
                | dir :: newRemDirs ->
                    if dir = PN && adjacencyFlags &&& Direction.PN = Direction.None then
                        Some (position + { R = -1; C =  0 })
                    elif dir = PS && adjacencyFlags &&& Direction.PS = Direction.None then
                        Some (position + { R = 1; C = 0 })
                    elif dir = PW && adjacencyFlags &&& Direction.PW = Direction.None then
                        Some (position + { R = 0; C = -1 })
                    elif dir = PE && adjacencyFlags &&& Direction.PE = Direction.None then
                        Some (position + { R = 0; C = 1 })
                    else 
                        tryStep newRemDirs
                | [] -> None
                        
            if adjacencyFlags = Direction.None then
                None
            else
                tryStep proposedDirs
                
        let mutable anyMoved = false
                 
        let moveOutcomes =
            elves
            |> Seq.map (fun elf -> elf, consider elf dirStream.Value)
            |> Seq.choose (fun (elf, proposed) -> proposed |> Option.map (fun p -> elf, p))
            |> Seq.groupBy snd
            |> Seq.map (mapSnd (fun g -> g |> Seq.map fst |> List.ofSeq))
            |> Seq.collect (fun (dest, elves) ->
                match elves with
                | [ e ] ->
                    anyMoved <- true
                    [ e, Choice1Of2 dest ]
                | es ->
                    es |> List.map (fun e -> e, Choice2Of2 es))
            |> Map.ofSeq

        let newElves =
            elves
            |> Seq.map (fun elf ->
                match moveOutcomes |> Map.tryFind elf with
                | Some (Choice1Of2 v) -> v
                | Some (Choice2Of2 _) -> elf
                | None -> elf)
            |> Set.ofSeq
        
        let shouldStop =
            match stopAtRound with
            | Some n -> roundN = n
            | None -> not anyMoved
  
        if shouldStop
        then newElves, roundN
        else doRound newElves dirStream.Next (roundN + 1) stopAtRound
                
    let part1 () =
        let elves = parseElves ()
        let finalElves, _ = doRound elves proposedDirectionStream 1 (Some 10)
        
        let min =
            let minR = finalElves |> Seq.map (fun e -> e.R) |> Seq.min
            let minC = finalElves |> Seq.map (fun e -> e.C) |> Seq.min
            { R = minR; C = minC }

        let max =
            let maxR = finalElves |> Seq.map (fun e -> e.R) |> Seq.max
            let maxC = finalElves |> Seq.map (fun e -> e.C) |> Seq.max
            { R = maxR; C = maxC }
        
        let result = int64 (max.R - min.R + 1L) * (max.C - min.C + 1L) - int64 elves.Count
        printfn $"%d{result}"
        
    let part2 () =
        let elves = parseElves ()
        let _, finalRound = doRound elves proposedDirectionStream 1 None
        printfn $"final round %d{finalRound}"
