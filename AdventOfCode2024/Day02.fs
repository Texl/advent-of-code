/// https://adventofcode.com/2024/day/2
module AdventOfCode2024.Day02

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day02.txt"

let testData =
   [
      "7 6 4 2 1"
      "1 2 7 8 9"
      "9 7 6 2 1"
      "1 3 2 4 5"
      "8 6 4 4 1"
      "1 3 6 7 9"      
   ]

let reports =
   data
   |> List.ofSeq
   |> List.map (String.split " " >> Array.toList >> List.map int)

let isSafe report =
   let deltas = report |> List.pairwise |> List.map (fun (a, b) -> b - a)
   let increasing = deltas |> List.forall (fun x -> x >= 0)
   let decreasing = deltas |> List.forall (fun x -> x <= 0)
   let inRange = deltas |> List.forall (abs >> inRangeInclusive 1 3)
   (increasing || decreasing) && inRange

let part1 () =
   reports |> List.filter isSafe |> List.length

let reportSubsets (report : _ list) =
   seq {
      for i in 0 .. report.Length - 1 do
         yield report |> List.removeAt i
   }

let part2 () =
   reports |> List.filter (reportSubsets >> Seq.exists isSafe) |> List.length
  
let part1Expected = 483

let part2Expected = 528
