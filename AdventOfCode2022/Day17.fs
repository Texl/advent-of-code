namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/17
module Day17 =
    let data = EmbeddedResource.loadText "Data/Day17.txt"

    let getShapePoints (strings : string list) =
        strings
        |> Seq.collecti (fun row str ->
            str
            |> Seq.mapi (fun col ->
                function
                | '#' -> Some { R = row; C = col }
                | ' ' -> None
                | _ -> raise invalidInput)
            |> Seq.choose id)
        |> List.ofSeq
    
    type Shape =
        { Points : Vector2 list
          Dimensions : Vector2 }
    
    let parseShape strs =
        { Points = getShapePoints strs
          Dimensions = { R = strs |> List.length |> int64; C = strs |> List.map String.length |> List.max |> int64 } }
    
    let shapePattern =
        [
            [ "####" ]
 
            [ " # "
              "###"
              " # " ]
 
            [ "  #"
              "  #"
              "###" ]
 
            [ "#"
              "#"
              "#"
              "#" ]
 
            [ "##"
              "##" ]
        ]
        |> List.map parseShape 
   
    type JetDirection =
        | Left
        | Right
    
    let jetPattern =
        // ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
        data
        |> Seq.head
        |> Seq.map (function
            | '>' -> Right
            | '<' -> Left
            | _ -> raise invalidInput)

    let shapes = (fun _ -> shapePattern |> Seq.indexed) |> Seq.initInfinite |> Seq.collect id |> ReadPoint.ofSeq
    
    let jets = (fun _ -> jetPattern |> Seq.indexed) |> Seq.initInfinite |> Seq.collect id |> ReadPoint.ofSeq

    type ShapeInstance =
        { Shape : Shape
          ShapeIndex : int
          Position : Vector2 }
        member this.TransformedPoints = this.Shape.Points |> List.map (fun p -> p + this.Position)
        member this.Left = this.Position.C
        member this.Right = this.Position.C + this.Shape.Dimensions.C - 1L
        member this.Top = this.Position.R
        member this.Bottom = this.Position.R + this.Shape.Dimensions.R - 1L
     
    let spawn floorLevel shapeIndex shape =
        let spawnOffset = { R = -3; C = 2 }
        { ShapeIndex = shapeIndex
          Shape = shape
          Position = spawnOffset + { R = floorLevel - shape.Dimensions.R + 1L; C = 0L } }
    
    type State =
        { FallingShape : ShapeInstance
          FallenShapesR : ShapeInstance list
          FloorLevel : int64
          FloorLevels : int64 list
          ShapeStream : ReadPoint<int * Shape>
          JetStream : ReadPoint<int * JetDirection>
          Tick : int64
          RunToPieceCount : int64
          KnownCycles : (int64 * int64) option
          KnownRemainder : int64 option
          PossibleHeightFound : int64 option }
    
    let getInitialState runToPieceCount =
        { FallingShape = shapes.Value ||> spawn 0 
          FallenShapesR = []
          FloorLevel = 0
          FloorLevels = 0L |> List.replicate 7
          ShapeStream = shapes.Next
          JetStream = jets
          Tick = 0
          RunToPieceCount = runToPieceCount
          KnownCycles = None
          KnownRemainder = None
          PossibleHeightFound = None }

    let tryOverlapAABB (a : ShapeInstance) (b : ShapeInstance) =
        a.Left <= b.Right &&
        a.Right >= b.Left &&
        a.Top <= b.Bottom &&
        a.Bottom >= b.Top

    let tryOverlap (fallingShape : ShapeInstance) (fallenShapesR : ShapeInstance list) =
        
        if fallingShape.Bottom > 0 || fallingShape.Left < 0 || fallingShape.Right >= 7 then
            true
        else
            let fallingPoints = fallingShape.TransformedPoints 
            
            let candidateShapes = fallenShapesR |> Seq.truncate 50
            
            candidateShapes
            |> Seq.exists (fun candidateShape ->
                if tryOverlapAABB fallingShape candidateShape then
                    fallingPoints |> Seq.exists (fun p1 -> candidateShape.TransformedPoints |> Seq.exists (fun p2 -> p1 = p2))
                else
                    false)
            
            // fallingShape.TransformedPoints
            // |> Seq.exists (fun p1 ->
            //     fallenShapesR
            //     |> Seq.truncate 50
            //     |> Seq.exists (fun f -> f.TransformedPoints |> Seq.exists (fun p2 -> p1 = p2)))

    let translate (translation : Vector2) (state : State) (shape : ShapeInstance) =
        let translatedShape = { shape with Position = shape.Position + translation }
        
        if shape.Bottom < state.FloorLevel - 1L then
            // cheaper
            if translatedShape.Left < 0 || translatedShape.Right >= 7
            then translation.R > 0, shape
            else false, translatedShape
        else
            match tryOverlap translatedShape state.FallenShapesR with
            | true -> translation.R > 0, shape
            | false -> false, translatedShape

    let solve initialState =
        let mutable hashes = Map.empty
    
        initialState
        |> Seq.unfold (fun state ->
            
            if state.PossibleHeightFound.IsNone then

                let hash =
                    let shapeIndex = state.FallingShape.ShapeIndex
                    let jetDirectionIndex = state.JetStream.Value |> fst
                    let floorLevels = state.FloorLevels |> List.map (fun fl -> state.FloorLevel - fl)
                    shapeIndex, jetDirectionIndex, floorLevels

                hashes <-
                    hashes
                    |> Map.change hash (function
                        | Some (pieceCounts : int64 list, floorLevels : int64 list) ->
                            let newFloorLevels = abs (state.FloorLevel - 1L) :: floorLevels
                            let newPieceCounts = int64 state.FallenShapesR.Length :: pieceCounts
                            Some (newPieceCounts, newFloorLevels)
                        | None -> Some ([ state.FallenShapesR.Length ], [ abs (state.FloorLevel - 1L) ]))
                
                let populatedHashes = hashes.Values |> Seq.filter (fun (shapes, heights) -> shapes.Length > 1 && heights.Length > 1) |> Seq.tryHead
                
                let newKnownCycles =
                    match populatedHashes with
                    | Some (shapes, heights) ->
                        opt {
                            let! a = shapes |> Seq.pairwise |> Seq.map (fun (a, b) -> a - b) |> Seq.distinct |> Seq.tryExactlyOne
                            let! b = heights |> Seq.pairwise |> Seq.map (fun (a, b) -> a - b) |> Seq.distinct |> Seq.tryExactlyOne
                            return a, b
                        }
                    | None -> state.KnownCycles
                
                let possibleHeightFound =
                    opt {
                        let! shapeCycle, heightCycle = newKnownCycles

                        let lookup = hashes |> Map.values |> Seq.filter (fun (l1, l2) -> l1.Length > 2 && l2.Length > 2) |> Seq.collect (fun t -> t ||> Seq.zip) |> Map.ofSeq
                        
                        if not lookup.IsEmpty then
                            let remainder =
                                let remainderBase = state.RunToPieceCount % shapeCycle
                                let multiple = (lookup.Keys |> Seq.min) / remainderBase
                                remainderBase + multiple * shapeCycle
                                    
                            let dividend = (state.RunToPieceCount - remainder) / shapeCycle
                            let height1 = dividend * heightCycle
                            let! height2 = lookup |> Map.tryFind remainder
                            return height1 + height2
                    }
                
                let rec fall (fallingShape : ShapeInstance) (jetStream : ReadPoint<int * JetDirection>) =
                    // drawState state.Tick fallingShape state.FallenShapesR
                    let jetTranslation =
                        match jetStream.Value with
                        | _, Left -> { R = 0; C = -1 } 
                        | _, Right -> { R = 0; C = 1 }
                        
                    let fallTranslation = { R = 1; C = 0 }

                    let shapeStopped, newFallingShape =
                        fallingShape
                        |> translate jetTranslation state |> snd
                        |> translate fallTranslation state
                            
                    if shapeStopped
                    then newFallingShape, jetStream.Next
                    else fall newFallingShape jetStream.Next

                let fallenShape, newJetStream = fall state.FallingShape state.JetStream
                
                let fallenShapePoints = fallenShape.TransformedPoints
                
                let newFloorLevel =
                    fallenShapePoints
                    |> Seq.map (fun p -> p.R)
                    |> Seq.min
                    |> min state.FloorLevel
                
                let newFloorLevels =
                    state.FloorLevels
                    |> Seq.mapi (fun ord floorLevel ->
                        let shapeMinFloorLevel =
                            fallenShapePoints
                            |> Seq.filter (fun p -> p.C = ord)
                            |> Seq.map (fun p -> p.R)
                            |> Seq.sort
                            |> Seq.tryHead 
                        
                        match shapeMinFloorLevel with
                        | Some v -> v |> min floorLevel
                        | None -> floorLevel)
                    |> List.ofSeq
                
                let newState =
                    { FallingShape = state.ShapeStream.Value ||> spawn (newFloorLevel - 1L)
                      FallenShapesR = fallenShape :: state.FallenShapesR
                      FloorLevel = newFloorLevel
                      FloorLevels = newFloorLevels
                      ShapeStream = state.ShapeStream.Next
                      JetStream = newJetStream
                      Tick = state.Tick + 1L
                      RunToPieceCount = state.RunToPieceCount
                      KnownCycles = newKnownCycles
                      KnownRemainder = state.KnownRemainder
                      PossibleHeightFound = possibleHeightFound }

                Some (newState, newState)
            else
                None)

    let part1 () =
        getInitialState 2022
        |> solve
        |> Seq.last
        |> (fun state ->
            state.PossibleHeightFound
            |> Option.iter (printfn "%d"))

    let part2 () =
        getInitialState 1_000_000_000_000L
        |> solve
        |> Seq.last
        |> (fun state ->
            state.PossibleHeightFound
            |> Option.iter (fun x ->
                // if x = 1547953216393L then printfn "OK!"
                printfn "%d" x))
