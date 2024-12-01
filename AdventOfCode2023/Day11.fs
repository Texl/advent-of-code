/// https://adventofcode.com/2023/day/11
module AdventOfCode2023.Day11

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day11.txt"

let rec pairs xs =
   seq {
      match xs with
      | a :: bs ->
         yield! bs |> List.map (fun b -> a, b)
         yield! pairs bs
      | [] -> ()
   }

let grid =
   data
   |> Seq.mapi (fun r row ->
      row
      |> Seq.mapi (fun c ->
         function
         | '#' -> Some { R = r; C = c }
         | _ -> None)
      |> Array.ofSeq)
   |> Array.ofSeq

let galaxies = grid |> Seq.collect id |> Seq.choose id |> List.ofSeq

let emptyRows, emptyCols =
   let empties xss =
      xss
      |> Seq.indexed
      |> Seq.filter (fun (_, xs) -> xs |> Seq.forall Option.isNone)
      |> Seq.map (fst >> int64)
      |> Array.ofSeq

   grid |> empties,
   grid |> Seq.transpose |> empties

let distance (expansionFactor : int64) pair =
   let a, b = pair
   let minR, minC = min a.R b.R, min a.C b.C
   let maxR, maxC = max a.R b.R, max a.C b.C
   let emptiesInPath =
      (emptyCols |> Seq.filter (fun c -> minC <= c && c <= maxC) |> Seq.length) +
      (emptyRows |> Seq.filter (fun r -> minR <= r && r <= maxR) |> Seq.length)

   (maxR - minR) + (maxC - minC) + (expansionFactor - 1L) * (int64 emptiesInPath)

let part1 () = galaxies |> pairs |> Seq.map (distance 2L) |> Seq.sum

let part2 () = galaxies |> pairs |> Seq.map (distance 1_000_000L) |> Seq.sum

let part1Expected = 9_509_330L

let part2Expected = 635_832_237_682L
