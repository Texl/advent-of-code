/// https://adventofcode.com/2023/day/13
module AdventOfCode2023.Day13

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day13.txt"

let grids =
   data
   |> List.ofSeq
   |> List.split String.isEmpty
   |> List.map (Seq.map List.ofSeq >> List.ofSeq)

let processGrid allowedDiffs (grid : char list list) =
   [ for i in 1 .. grid.Length - 1 -> i ]
   |> List.map (fun index ->
      index,
      grid
      |> List.splitAt index
      |> fun (front, back) ->
         (front |> List.rev, back)
         ||> Seq.map2 (Seq.map2 (fun a b -> if a <> b then 1 else 0))
      |> Seq.sumBy Seq.sum)
   |> Seq.filter (fun (_, diffs) -> diffs = allowedDiffs)
   |> Seq.map fst
   |> Seq.tryExactlyOne
   |> Option.defaultValue 0

let processGrids numDiffs grid =
   let rowsAbove = grid |> processGrid numDiffs
   let colsLeft = grid |> List.transpose |> processGrid numDiffs
   100 * rowsAbove + colsLeft

let part1 () = grids |> Seq.sumBy (processGrids 0)

let part2 () = grids |> Seq.sumBy (processGrids 1)

let part1Expected = 35538

let part2Expected = 30442
