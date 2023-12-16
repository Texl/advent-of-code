namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/12
module Day12 =

    let data = EmbeddedResource.loadText "Data/Day12.txt"

    type GridCellType = Start | End | Elevation of int
    type Coords = int * int
    type GridCell = { Type : GridCellType; Coords : Coords }

    module Grid =
        let private grid =
            data
            |> Seq.mapi (fun r row ->
                row
                |> Seq.mapi (fun c ->
                    function
                    | 'S' -> { Type = Start; Coords = r, c }
                    | 'E' -> { Type = End; Coords = r, c }
                    | elev -> { Type = Elevation (int elev - int 'a'); Coords = r, c }) // elevation range [0, 25]
                |> Array.ofSeq)
            |> Array.ofSeq

        let isStartCell (cell : GridCell) = cell.Type = Start
        let isEndCell (cell : GridCell) = cell.Type = End
        let isElevationCell elevation (cell : GridCell) = cell.Type = Elevation elevation
        let isValidCoords ((r, c) : Coords) : bool = r >= 0 && r < grid.Length && c >= 0 && c < grid[0].Length
        let findCell (f : GridCell -> bool) : GridCell = grid |> Array.collect id |> Array.find f

        let private getNeighbors isTraversable ({ Coords = r, c } as fromGridCell : GridCell)  =
            [ r - 1, c
              r + 1, c
              r, c - 1
              r, c + 1 ]
            |> Seq.filter isValidCoords
            |> Seq.map (fun (r, c) -> grid[r][c])
            |> Seq.filter (isTraversable fromGridCell)
        
        let flood isTraversable startCellCondition stopCellCondition =
            let rec floodFillRec (newFlooded, flooded) steps =
                let newNeighbors = (newFlooded |> Seq.collect (getNeighbors isTraversable) |> Set.ofSeq) - flooded
                if newNeighbors |> Seq.exists stopCellCondition
                then steps
                else floodFillRec (newNeighbors, flooded + newNeighbors) (steps + 1)
            let start = findCell startCellCondition |> Set.singleton
            floodFillRec (start, start) 1

    let isTraversable (fromCell : GridCell) (toCell : GridCell) = 
        match fromCell.Type, toCell.Type with
        | Start, Elevation 0 -> true
        | Elevation 25, End -> true
        | Elevation elevFrom, Elevation elevTo -> elevTo - elevFrom <= 1
        | _ -> false
                    
    let part1 () =
        Grid.flood isTraversable Grid.isStartCell Grid.isEndCell
        |> printfn "%d"
    
    let part2 () =
        Grid.flood (flip isTraversable) Grid.isEndCell (Grid.isElevationCell 0)
        |> printfn "%d"
