namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/22
module Day22 =
    let data = EmbeddedResource.loadText "Data/Day22.txt"
    
    let el = 50
        
    type MapCellType =
        | Void
        | Open
        | Wall

    type Facing =
        | Right
        | Down
        | Left
        | Up

    type MapCell =
        { CubeFace : int
          FaceCoords :Vector2
          Coords : Vector2
          Type : MapCellType
          mutable Connections : Map<Facing, MapCell * Facing> }
    
    type Direction =
        | MoveForward of int
        | TurnLeft
        | TurnRight
        
    let inline modulo a b = (a % b + b) % b

    let parseInput (linkMapCells : MapCell list list -> Map<int, MapCell list list> -> Map<int, Map<Vector2, MapCell>> -> Map<Vector2, MapCell> -> unit) =
        let parseMap lines classifyCubeFace =
            let mapCells =
                lines
                |> Seq.mapi (fun r row ->
                    row
                    |> Seq.mapi (fun c ch ->
                        let coords = { R = r; C = c }
                        let cellType =
                            match ch with
                            | ' ' -> Void
                            | '.' -> Open
                            | '#' -> Wall
                            | _ -> raise invalidInput
                            
                        let cubeFace = classifyCubeFace r c el

                        let cr, cc = modulo r el, modulo c el

                        let faceCoords = { R = cr; C = cc }
                                            
                        { CubeFace = cubeFace
                          FaceCoords = faceCoords
                          Coords = coords
                          Type = cellType
                          Connections = Map.empty })
                    |> List.ofSeq)
                |> List.ofSeq
            
            let findStart (mapCells : MapCell list list) =
                mapCells.Head |> List.find (fun cell -> cell.Type = Open)
        
            let faceToMapCells =
                mapCells
                |> Seq.collect (Seq.map (fun cell -> (cell.CubeFace, cell.FaceCoords), cell))
                |> Seq.groupBy (fst >> fst)
                |> Seq.map (mapSnd (fun x ->
                    x
                    |> Seq.groupBy (fun ((_, x), _) -> x.R)
                    |> Seq.map (fun (_, xx) -> xx |> (fun x -> x |> Seq.sortBy (fun ((_, y), _) -> y.C)) |> Seq.map snd |> List.ofSeq)
                    |> List.ofSeq))
                |> Map.ofSeq
                
            let faceToCoordsToMapCell =
                faceToMapCells
                |> Map.filter (fun k _ -> k <> -1)
                |> Map.map (fun _ cells ->
                    cells
                    |> Seq.collect (Seq.map (fun cell -> cell.FaceCoords, cell))
                    |> Map.ofSeq)

            let start = findStart mapCells
                        
            let cellsByCoords =
                mapCells
                |> Seq.collect (Seq.map (fun c -> c.Coords, c))
                |> Map.ofSeq
                
            linkMapCells mapCells faceToMapCells faceToCoordsToMapCell cellsByCoords
            
            start

        let parseDirections lines =
            let line = lines |> Seq.exactlyOne
            
            let (|ForwardDir|_|) =
                function
                | Int n -> Some (ForwardDir MoveForward n)
                | _ -> None
            
            let (|LeftRightDir|_|) =
                function
                | "L" -> Some TurnLeft
                | "R" -> Some TurnRight
                | _ -> None
            
            let rec parseDirections str soFarR =
                match str with
                | Regex "(\d*)(.*)" [ ForwardDir n; rest ] -> parseDirections rest (n :: soFarR)
                | Regex "(\w)(.*)" [ LeftRightDir dir; rest ] -> parseDirections rest (dir :: soFarR)
                | "" -> soFarR |> List.rev
                | junk -> failwithf "%s" junk

            parseDirections line []
        
        let classifyCubeFace el r c =
            if   r >= 0 * el && r < 1 * el && c >= 1 * el && c < 2 * el then 0
            elif r >= 0 * el && r < 1 * el && c >= 2 * el && c < 3 * el then 1
            elif r >= 1 * el && r < 2 * el && c >= 1 * el && c < 2 * el then 2
            elif r >= 2 * el && r < 3 * el && c >= 0 * el && c < 1 * el then 3
            elif r >= 2 * el && r < 3 * el && c >= 1 * el && c < 2 * el then 4
            elif r >= 3 * el && r < 4 * el && c >= 0 * el && c < 1 * el then 5
            else -1

        data
        |> List.ofSeq
        |> List.split (fun str -> str = "")
        |> function
            | [ map; directions ] -> parseMap map classifyCubeFace, parseDirections directions
            | junk -> failwithf "Unexpected data: %A" junk
    
    let parseInputCube (linkMapCells : MapCell list list -> Map<int, MapCell list list> -> Map<int, Map<Vector2, MapCell>> -> Map<Vector2, MapCell> -> unit) =
        let parseMap lines el classifyCubeFace =
            let mapCells =
                lines
                |> Seq.mapi (fun r row ->
                    row
                    |> Seq.mapi (fun c ch ->
                        let coords = { R = r; C = c }
                        let cellType =
                            match ch with
                            | ' ' -> Void
                            | '.' -> Open
                            | '#' -> Wall
                            | _ -> raise invalidInput
                            
                        let cubeFace = classifyCubeFace r c el

                        let cr, cc = modulo r el, modulo c el

                        let faceCoords = { R = cr; C = cc }
                                            
                        { CubeFace = cubeFace
                          FaceCoords = faceCoords
                          Coords = coords
                          Type = cellType
                          Connections = Map.empty })
                    |> List.ofSeq)
                |> List.ofSeq
            
            let findStart (mapCells : MapCell list list) =
                mapCells.Head |> List.find (fun cell -> cell.Type = Open)
        
            let cellsByCoords =
                mapCells
                |> Seq.collect (Seq.map (fun c -> c.Coords, c))
                |> Map.ofSeq
                
            let faceToMapCells =
                mapCells
                |> Seq.collect (Seq.map (fun cell ->
                    (cell.CubeFace, cell.FaceCoords), cell))
                |> Seq.groupBy (fst >> fst)
                |> Seq.map (mapSnd (fun x ->
                    x
                    |> Seq.groupBy (fun ((_, x), _) -> x.R)
                    |> Seq.map (fun (_, xx) -> xx |> (fun x -> x |> Seq.sortBy (fun ((_, y), c) -> y.C)) |> Seq.map (fun ((_, z), zz)-> zz) |> List.ofSeq)
                    |> List.ofSeq))
                |> Map.ofSeq
                
            let faceToCoordsToMapCell =
                faceToMapCells
                |> Map.filter (fun k _ -> k <> -1)
                |> Map.map (fun _ cells ->
                    cells
                    |> Seq.collect (Seq.map (fun cell -> cell.FaceCoords, cell))
                    |> Map.ofSeq)

            let start = findStart mapCells
            
            linkMapCells mapCells faceToMapCells faceToCoordsToMapCell cellsByCoords
            
            start

        let parseDirections lines =
            let line = lines |> Seq.exactlyOne
            
            let (|ForwardDir|_|) =
                function
                | Int n -> Some (ForwardDir MoveForward n)
                | _ -> None
            
            let (|LeftRightDir|_|) =
                function
                | "L" -> Some TurnLeft
                | "R" -> Some TurnRight
                | _ -> None
            
            let rec parseDirections str soFarR =
                match str with
                | Regex "(\d*)(.*)" [ ForwardDir n; rest ] -> parseDirections rest (n :: soFarR)
                | Regex "(\w)(.*)" [ LeftRightDir dir; rest ] -> parseDirections rest (dir :: soFarR)
                | "" -> soFarR |> List.rev
                | junk -> failwithf "%s" junk

            parseDirections line []
        
        let classifyCubeFace el r c =
            if   r >= 0 * el && r < 1 * el && c >= 1 * el && c < 2 * el then 0
            elif r >= 0 * el && r < 1 * el && c >= 2 * el && c < 3 * el then 1
            elif r >= 1 * el && r < 2 * el && c >= 1 * el && c < 2 * el then 2
            elif r >= 2 * el && r < 3 * el && c >= 0 * el && c < 1 * el then 3
            elif r >= 2 * el && r < 3 * el && c >= 1 * el && c < 2 * el then 4
            elif r >= 3 * el && r < 4 * el && c >= 0 * el && c < 1 * el then 5
            else -1

        data
        |> List.ofSeq
        |> List.split (fun str -> str = "")
        |> function
            | [ map; directions ] -> parseMap map 50 classifyCubeFace, parseDirections directions
            | junk -> failwithf "Unexpected data: %A" junk
    
    let rec traverse (facing : Facing) (cell : MapCell) (directions : Direction list) =
        let rec moveForward n facing (cell : MapCell) =
            if n = 0 then
                cell, facing
            else
                let nextCell, newFacing = cell.Connections |> Map.find facing
                match nextCell.Type with
                | Void -> failwith "Shouldn't happen..."
                | Open -> moveForward (n - 1) newFacing nextCell
                | Wall -> cell, facing

        let turnLeft =
            function
            | Right -> Up
            | Down -> Right
            | Left -> Down
            | Up -> Left
            
        let turnRight =
            function
            | Right -> Down
            | Down -> Left
            | Left -> Up
            | Up -> Right

        match directions with
        | dir :: newDirections ->
            match dir with
            | MoveForward n ->
                let nextCell, newFacing = moveForward n facing cell
                traverse newFacing nextCell newDirections
            | TurnLeft ->
                traverse (turnLeft facing) cell newDirections
            | TurnRight ->
                traverse (turnRight facing) cell newDirections
        | [] -> cell, facing

    let getPassword cell facing =
        let facingScore =
            match facing with
            | Right -> 0L
            | Down -> 1L
            | Left -> 2L
            | Up -> 3L
        
        1000L * (cell.Coords.R + 1L) +
        4L * (cell.Coords.C + 1L) +
        facingScore

    let linkMapCells (mapCells : MapCell list list) (faceToMapCells : Map<int, MapCell list list>) _ (cellsByCoords : Map<Vector2, MapCell>) =
        let height, width = mapCells.Length, mapCells.Head.Length

        let wrapCoords coords =
            { R = int64 <| modulo (int coords.R) height
              C = int64 <| modulo (int coords.C) width }

        faceToMapCells
        |> Map.filter (fun k _ -> k <> -1)
        |> Map.iter (fun _ cells ->
            cells
            |> List.iter (fun row ->
                row
                |> List.iter (fun cell ->
                    let neighborDirections =
                        [ Right, { R = 0; C = 1 }
                          Down, { R = 1; C = 0 }
                          Left, { R = 0; C = -1 }
                          Up, { R = -1; C = 0 } ]
                    
                    let rec search (origin : Vector2) (direction : Vector2) =
                        let candidate = (origin + direction) |> wrapCoords
                        let nextCell = cellsByCoords |> Map.tryFind candidate
                        match nextCell with
                        | Some { Type = Void } -> search candidate direction 
                        | Some cell -> cell
                        | _ -> search candidate direction

                    let neighbors =
                        neighborDirections
                        |> List.map (fun (facing, translation) ->
                            let found = search cell.Coords translation
                            facing, (found, facing))
                        |> Map.ofSeq
                                    
                    cell.Connections <- neighbors)))

    let part1 () =
        let start, directions = parseInput linkMapCells
        let finalCell, finalFacing = traverse Right start directions
        printfn $"%d{getPassword finalCell finalFacing}"

    let part2 () =
        let transitionMap el facing cube =
            let identity = id
            
            let rev (coords : Vector2) =
                { R = int64 (el - 1) - coords.R; C = coords.C }
            
            let transpose (coords : Vector2) =
                { R = coords.C; C = coords.R }

            let rotateR = rev >> transpose
            
            let rotateL = transpose >> rev
            
            let rotate180 = rotateR >> rotateR

            match cube, facing with
            | 0, Right -> 1, Right, identity
            | 0, Down -> 2, Down, identity
            | 0, Left -> 3, Right, rotate180 
            | 0, Up -> 5, Right, rotateR
        
            | 1, Right -> 4, Left, rotate180
            | 1, Down -> 2, Left, rotateR
            | 1, Left -> 0, Left, identity
            | 1, Up -> 5, Up, identity
        
            | 2, Right -> 1, Up, rotateL
            | 2, Down -> 4, Down, identity
            | 2, Left -> 3, Down, rotateL
            | 2, Up -> 0, Up, identity
        
            | 3, Right -> 4, Right, identity
            | 3, Down -> 5, Down, identity
            | 3, Left -> 0, Right, rotate180
            | 3, Up -> 2, Right, rotateR
                    
            | 4, Right -> 1, Left, rotate180
            | 4, Down -> 5, Left, rotateR
            | 4, Left -> 3, Left, identity
            | 4, Up -> 2, Up, identity
            
            | 5, Right -> 4, Up, rotateL
            | 5, Down -> 1, Down, identity
            | 5, Left -> 0, Down, rotateL
            | 5, Up -> 3, Up, identity
            
            | _ -> failwith "invalid transition"

        let linkMapCells _ (faceToMapCells : Map<int, MapCell list list>) (faceToCoordsToMapCell : Map<int, Map<Vector2, MapCell>>) _ =
            faceToMapCells
            |> Map.filter (fun k _ -> k <> -1)
            |> Map.iter (fun _ cells ->
                cells
                |> List.iter (fun row ->
                    row
                    |> List.iter (fun cell ->
                        let neighborDirections =
                            [ Right, { R = 0; C = 1 }
                              Down, { R = 1; C = 0 }
                              Left, { R = 0; C = -1 }
                              Up, { R = -1; C = 0 } ]
                        
                        let isOutsideBounds faceCoords =
                            faceCoords.R < 0 ||
                            faceCoords.R >= el ||
                            faceCoords.C < 0 ||
                            faceCoords.C >= el
                        
                        let wrapCoords coords =
                            { R = int64 <| modulo (int coords.R) el
                              C = int64 <| modulo (int coords.C) el }

                        let rec search (originCell : MapCell) facing translation =
                            let newFaceCoords = originCell.FaceCoords + translation
                            
                            if isOutsideBounds newFaceCoords then
                                let newCubeFace, newFacing, transform = transitionMap el facing originCell.CubeFace
                                let wrappedFaceCoords = newFaceCoords |> wrapCoords |> transform
                                faceToCoordsToMapCell[newCubeFace][wrappedFaceCoords], newFacing
                            else
                                faceToCoordsToMapCell[originCell.CubeFace][newFaceCoords], facing

                        let neighbors =
                            neighborDirections
                            |> List.map (fun (facing, translation) ->
                                let found = search cell facing translation
                                facing, found)
                            |> Map.ofSeq
                
                        cell.Connections <- neighbors)))
        
        let start, directions = parseInputCube linkMapCells
        let finalCell, finalFacing = traverse Right start directions
        printfn $"%d{getPassword finalCell finalFacing}"
