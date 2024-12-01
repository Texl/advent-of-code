/// https://adventofcode.com/2023/day/18
module AdventOfCode2023.Day18

open System
open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day18.txt"

type Direction = N | W | S | E

let (|Direction|) str =
   match str with
   | "U" | "3" -> N
   | "L" | "2" -> W
   | "D" | "1" -> S
   | "R" | "0" -> E
   | x -> raiseInvalidInput x

let digPlan1, digPlan2 =
   data
   |> Seq.map (fun line ->
      match line |> String.split " " with
      | [| Direction dir1; Int64 distance1; Regex "\(#([0-9a-f]{5})([0-9a-f])\)" [ str; Direction dir2 ] |] ->
         (dir1, distance1), (dir2, Convert.ToInt64(str, 16))
      | x -> raiseInvalidInput x)
   |> Seq.unzip

let getDigPath digPlan = 
   (Vector2.Zero, digPlan)
   ||> Seq.scan (fun s (dir, dist) ->
      let delta = 
         match dir with
         | N -> -Vector2.UnitR
         | W -> -Vector2.UnitC
         | S -> Vector2.UnitR
         | E -> Vector2.UnitC
      s + dist * delta)
   |> Array.ofSeq

// (Shoelace formula)[https://en.wikipedia.org/wiki/Shoelace_formula]
// (Pick's theorem)[https://en.wikipedia.org/wiki/Pick%27s_theorem]
let findDigPathArea (digPathVerts : Vector2[]) =
   let pathSegments = digPathVerts |> Array.pairwise
   let interiorArea = (pathSegments |> Seq.sumBy (fun (p1, p2) -> p1.C * p2.R - p2.C * p1.R)) / 2L
   let perimeter = pathSegments |> Seq.sumBy Vector2.ManhattanDistance
   abs interiorArea + perimeter / 2L + 1L

let part1 () = digPlan1 |> getDigPath |> findDigPathArea
let part2 () = digPlan2 |> getDigPath |> findDigPathArea

let part1Expected = 92_758L

let part2Expected = 62_762_509_300_678L
