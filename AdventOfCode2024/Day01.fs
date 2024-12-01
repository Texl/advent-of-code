/// https://adventofcode.com/2024/day/1
module AdventOfCode2024.Day01

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day01.txt"

let testData =
   [
      "3   4"
      "4   3"
      "2   5"
      "1   3"
      "3   9"
      "3   3"
   ]

let xs, ys =
   data
   |> Seq.map (fun line ->
      match line |> String.split " " with
      | [| Int x; Int y |] -> x, y
      | _ -> raiseInvalidInput line)
   |> Seq.toArray
   |> Array.unzip

let part1 () =
   (xs |> Array.sort, ys |> Array.sort)
   ||> Seq.map2 (fun x y -> abs (x - y))
   |> Seq.sum

let part2 () =
   let counts = ys |> Seq.countBy id |> Map.ofSeq

   xs
   |> Seq.sumBy (fun x ->
      let count = counts |> Map.tryFind x |> Option.defaultValue 0
      x * count)

let part1Expected = 3569916

let part2Expected = 26407426
