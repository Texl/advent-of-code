/// https://adventofcode.com/2024/day/19
module AdventOfCode2024.Day19

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day19.txt"

let testData =
   seq {
      "r, wr, b, g, bwu, rb, gb, br"
      ""
      "brwrr"
      "bggr"
      "gbbr"
      "rrbgbr"
      "ubwu"
      "bwurrg"
      "brgr"
      "bbrgwb"      
   }

let patterns, designs =
   data
   |> Seq.toList
   |> List.split String.isEmpty
   |> function
      | [ [ patternsData ]; designs ] -> patternsData |> String.split ", ", designs
      | _ -> raise invalidInput

let countM =
   Func.memoizeRec (fun countRec (design : string) ->
      if design |> String.isEmpty
      then 1L
      else patterns |> Seq.filter design.StartsWith |> Seq.sumBy (fun pattern -> countRec design[pattern.Length..]))

let part1 () = designs |> Seq.filter (countM >> (<>) 0) |> Seq.length

let part2 () = designs |> Seq.sumBy countM

let part1Expected = 236

let part2Expected = 643685981770598L
