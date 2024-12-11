/// https://adventofcode.com/2024/day/11
module AdventOfCode2024.Day11

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day11.txt"

let testData =
   seq {
      "125 17"
   }
   
let initialStoneCounts : Map<int64, int64> =
   data
   |> Seq.exactlyOne
   |> String.split " "
   |> Seq.map int64
   |> Seq.countBy id
   |> Map.ofSeq
   |> Map.map (fun _ -> int64)

let add key delta = Map.change key (function None -> Some delta | Some current -> Some (current + delta)) 

let applyRules (stoneCounts : Map<int64, int64>) =
   (Map.empty, stoneCounts |> Map.toSeq)
   ||> Seq.fold (fun acc (stone, count) ->
      if stone = 0L then
         acc |> add 1L count
      else
         let cellStr = string stone
         if cellStr.Length % 2 = 0 then
            let n1, n2 = cellStr |> String.splitAt (cellStr.Length / 2) |> mapBoth int64
            acc
            |> add n1 count 
            |> add n2 count 
         else
            acc |> add (stone * 2024L) count)

let part1 () = initialStoneCounts |> Func.repeat 25 applyRules |> Seq.sumBy _.Value
   
let part2 () = initialStoneCounts |> Func.repeat 75 applyRules |> Seq.sumBy _.Value

let part1Expected = 239714L

let part2Expected = 284973560658514L
