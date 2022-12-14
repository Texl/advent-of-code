namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/14
module Day14 =
    let rawData = EmbeddedResource.loadText "Data/Day14.txt"

    type GridCellType =
        | Air
        | Rock
        | Sand
        | FallingSand
        
    type FallingSandSimulation (inputData : seq<string>, addFloor : bool) =
        let sandSpawnLocation = 0, 500
        
        let getExtentsFromHeight height =
            let _, sandC = sandSpawnLocation 
            (0, sandC - height - 1), (height, sandC + height + 1)

        let lines =
            let parseCoords (str : string) =
                match str.Split(",") with
                | [| Int c; Int r |] -> (r, c)
                | _ -> raise invalidInput
            
            inputData
            |> Seq.map (fun str -> str.Split(" -> ") |> Seq.map parseCoords)
            |> Seq.collect (fun s -> s |> Seq.pairwise)
            |> List.ofSeq

        let maxR = lines |> List.unzip ||> (@) |> Seq.map fst |> Seq.max
        let extents = getExtentsFromHeight (maxR + (if addFloor then 2 else 0))
        let (minR, minC), (maxR, maxC) = extents
        let width = maxC - minC + 1
        let height = maxR - minR + 1
        let mapCoord (r, c) =
            r - minR, c - minC

        let sandSpawn = mapCoord sandSpawnLocation
        
        let grid =
            Array.init height (fun _ ->
                Array.init width (fun _ ->
                    Air))
            
        let isValidCoords (r, c) = r >= 0 && r < grid.Length && c >= 0 && c < grid[0].Length
            
        let linesToDraw =
            if addFloor
            then lines @ [ (maxR, minC), (maxR, maxC) ]
            else lines
            
        let tryGetCell (r, c as coords) =
            if isValidCoords coords
            then Some (grid[r][c])
            else None

        do for (r1, c1), (r2, c2) in linesToDraw do
            let dr = abs (r2 - r1) + 1
            let dc = abs (c2 - c1) + 1
            
            let mnR = min r1 r2
            let mnC = min c1 c2
            
            if dr = 1 then
                for i in 0 .. dc - 1 do
                    let r = mnR - minR
                    let c = mnC + i - minC
                    grid[r][c] <- Rock
                    
            elif dc = 1 then
                for i in 0 .. dr - 1 do
                    let r = mnR + i - minR
                    let c = mnC - minC
                    grid[r][c] <- Rock

        member this.PrintGrid () =
            grid
            |> Array.map (fun row ->
                row
                |> Array.map (fun cell ->
                    match cell with
                    | Air ->  "."
                    | Rock -> "#"
                    | Sand -> "o"
                    | FallingSand -> "~")
                |> String.concat "")
            |> String.concat "\n"
            |> printfn "%s"
        
        member this.Run () =
            let rec fall (r, c) accSand =
                grid[r][c] <- FallingSand

                let possibleMove =
                    [ r + 1, c
                      r + 1, c - 1
                      r + 1, c + 1 ]
                    |> List.choose (fun coords ->
                        match tryGetCell coords with
                        | Some Air -> Some (Choice1Of2 coords) // air - legal move
                        | Some _ -> None // some obstacle
                        | None -> Some (Choice2Of2 coords)) // out of bounds - legal move
                    |> List.tryHead
                    
                match possibleMove with
                | Some (Choice1Of2 coords) ->
                    grid[r][c] <- Air // fall into air
                    fall coords accSand
                | Some (Choice2Of2 _) ->
                    accSand // fall out of bounds
                | None ->
                    if (r, c) = sandSpawn then
                        ((r, c) :: accSand) // settled at spawn
                    else
                        grid[r][c] <- Sand
                        fall sandSpawn ((r, c) :: accSand) // settled elsewhere

            fall sandSpawn []

    let part1 () =
        let simulation = FallingSandSimulation(rawData, addFloor=false)
        let accumulatedSand = simulation.Run()
        accumulatedSand.Length |> printfn "%d"

    let part2 () =
        let simulation = FallingSandSimulation(rawData, addFloor=true)
        let accumulatedSand = simulation.Run()
        accumulatedSand.Length |> printfn "%d"
