namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/24
module Day24 =
    let data = EmbeddedResource.loadText "Data/Day24.txt"

    let testData =
        [ "#.######"
          "#>>.<^<#"
          "#.<..<<#"
          "#>v.><>#"
          "#<^v^^>#"
          "######.#" ]

    type Blizzard =
        { Position : Vector2
          Direction : Vector2 }
    
    type Map =
        { Start : Vector2
          End : Vector2
          Dimensions : Vector2
          Blizzards : Blizzard list
          Period : int }
        member this.Print (overlay : Set<Vector2>, overlayCharacter : string) =
            for r in 0L .. this.Dimensions.R - 1L do
                for c in 0L .. this.Dimensions.C - 1L do
                    let position = { R = int64 r; C = int64 c }
                    if overlay |> Set.contains position then
                        printf $"%s{overlayCharacter}"
                    elif position = this.Start then
                        printf "S"
                    elif position = this.End then
                        printf "E"
                    elif position.R = 0 || position.C = 0 || position.R = this.Dimensions.R - 1L || position.C = this.Dimensions.C - 1L then
                        printf "#"
                    elif this.Blizzards |> List.exists (fun blizzard -> blizzard.Position = position) then
                        let blizz = this.Blizzards |> List.find (fun blizzard -> blizzard.Position = position)
                        match blizz.Direction with
                        | { R = -1L; C = 0L } -> printf "^"
                        | { R = 1L; C = 0L } -> printf "v"
                        | { R = 0L; C = -1L } -> printf "<"
                        | { R = 0L; C = 1L } -> printf ">"
                        | _ -> failwith "impossible"
                    else
                        printf "."
                printfn ""
            printfn ""
    
    type SearchHead =
        { Position : Vector2
          Minutes : int
          MapStream : ReadPoint<Map> }
        
    type Context =
        { Position : Vector2
          CycleIndex : int }

    type InputContext =
        { Initial : SearchHead
          StartPoint : Vector2
          EndPoint : Vector2
          Solve : Vector2 -> SearchHead -> SearchHead }
        
    let getInputContext (inputData : seq<string>) : InputContext =

        let input =
            let lines = inputData |> List.ofSeq
            
            let first = lines.Head
            
            let mid = lines |> List.skip 1 |> List.take (lines.Length - 2)
            
            let last = lines |> List.last

                    
            let startPoint = { R = 0; C = first.IndexOf('.') }
            let endPoint = { R = int64 (lines.Length - 1); C = last.IndexOf('.') }

            let dimensions = { R = int64 (mid.Length + 2); C = int64 mid.Head.Length }
            
            let blizzards =
                mid
                |> Seq.collecti (fun r ->
                    Seq.mapi (fun c ->
                        function
                        | '.' -> None
                        | '#' -> None
                        | '^' -> { Position = { R = int64 (r + 1); C = c }
                                   Direction = { R = -1; C = 0 } } |> Some
                        | 'v' -> { Position = { R = int64 (r + 1); C = c }
                                   Direction = { R = 1; C = 0} } |> Some
                        | '<' -> { Position = { R = int64 (r + 1); C = c }
                                   Direction = { R = 0; C = -1 } } |> Some
                        | '>' -> { Position = { R = int64 (r + 1); C = c }
                                   Direction = { R = 0; C = 1 } } |> Some
                        | _ -> raise invalidInput))
                |> Seq.choose id
                |> List.ofSeq
            
            let period = int (lcm (dimensions.R - 2L) (dimensions.C - 2L))

            { Start = startPoint
              End = endPoint
              Dimensions = dimensions
              Blizzards = blizzards
              Period = period }

        let step (map : Map) =
            let wrap position =
                { R = (position.R - 1L) %% (map.Dimensions.R - 2L) + 1L
                  C = (position.C - 1L) %% (map.Dimensions.C - 2L) + 1L }
            
            let newBlizzards =
                map.Blizzards
                |> Seq.map (fun blizzard ->
                    { blizzard with Position = wrap (blizzard.Position + blizzard.Direction) })
                |> List.ofSeq
                
            { map with Blizzards = newBlizzards }

        let isTraversable map position =
            if position = map.Start || position = map.End then
                true
            elif map.Blizzards |> List.exists (fun blizzard -> blizzard.Position = position) then
                false
            else
                position.R >= 1L &&
                position.R <= map.Dimensions.R - 2L &&
                position.C >= 1L &&
                position.C <= map.Dimensions.C - 2L
            
        let getNeighbors map position =
            [ { R = 0; C = 0 }
              { R = 0; C = 1 }
              { R = 1; C = 0 }
              { R = 0; C = -1 }
              { R = -1; C = 0 } ]
            |> List.choose (fun direction ->
                let destination = position + direction
                if isTraversable map destination
                then Some destination
                else None)

        // let getMapStream () =
        //     let initialMap = input
        //     
        //     let mapCycle =
        //         initialMap
        //         |> Seq.unfold (fun map -> Some (map, step map))
        //         |> Seq.truncate initialMap.Period
        //         
        //     (fun _  -> mapCycle)
        //     |> Seq.initInfinite
        //     |> Seq.collect id
        //     |> ReadPoint.ofSeq

        let search (initialHead : SearchHead) pEnd =
            
            let mutable best = Map.empty

            let rec solve (queue : SearchHead list) =
                match queue with
                | h :: remaining ->
                    if h.Position = pEnd then
                        h
                    else
                        let context = { Position = h.Position; CycleIndex = h.Minutes % (int h.MapStream.Value.Period) }
                        match best |> Map.tryFind context with
                        | Some existing when h.Minutes >= existing ->
                            solve remaining
                        | _ ->
                            best <- best |> Map.add context h.Minutes

                            let neighbors = getNeighbors h.MapStream.Value h.Position
                            
                            // printMap h.MapStream.Value (Set.ofList neighbors)
                            
                            let heads =
                                neighbors
                                |> List.map (fun n ->
                                    { Position = n
                                      Minutes = h.Minutes + 1
                                      MapStream = h.MapStream.Next })
                                
                            solve (remaining @ heads)
                | [] -> failwith "error"

            solve [ initialHead ]

        let solve2 (endPoint : Vector2) (initialSearchHead : SearchHead) : SearchHead =
            search initialSearchHead endPoint
        
        let mapStream =
            let initialMap = input
            
            let mapCycle =
                initialMap
                |> Seq.unfold (fun map -> Some (map, step map))
                |> Seq.truncate initialMap.Period
                
            (fun _  -> mapCycle)
            |> Seq.initInfinite
            |> Seq.collect id
            |> ReadPoint.ofSeq

        let startPoint = mapStream.Value.Start
        
        let endPoint = mapStream.Value.End
                
        let initial =
            { Position = startPoint
              Minutes = 0
              MapStream = mapStream.Next }
        
        { Initial = initial
          StartPoint = startPoint
          EndPoint = endPoint
          Solve = solve2 }
    
    let ctx = getInputContext data
    
    let part1 () =
        let result =
            ctx.Initial
            |> ctx.Solve ctx.EndPoint

        result.Minutes
        
        // ----
        
        // let mapStream = getMapStream ()
        //
        // let pStart = mapStream.Value.Start
        // let pEnd = mapStream.Value.End
        // let initial =
        //     { Position = pStart
        //       Minutes = 0
        //       MapStream = mapStream.Next }
        //
        // let result = search initial pStart pEnd
        //
        // result |> printfn "%A"
         
    let part1Result = 262

    let part2 () =
        let result =
            ctx.Initial
            |> ctx.Solve ctx.EndPoint
            |> ctx.Solve ctx.StartPoint
            |> ctx.Solve ctx.EndPoint

        result.Minutes
    
        // let mapStream = getMapStream ()
        //
        // let pStart = mapStream.Value.Start
        // let pEnd = mapStream.Value.End
        // let initial =
        //     { Position = pStart
        //       Minutes = 0
        //       MapStream = mapStream.Next }
        //
        // let result = search initial pStart pEnd
        // let result2 = search result pEnd pStart
        // let result3 = search result2 pStart pEnd
        // result3.Minutes |> printfn "%A"
        //
    let part2Result = 785
