/// https://adventofcode.com/2024/day/8
module AdventOfCode2024.Day08

open AdventOfCode.Common

let testData =
   [
      "............"
      "........0..."
      ".....0......"
      ".......0...."
      "....0......."
      "......A....."
      "............"
      "............"
      "........A..."
      ".........A.."
      "............"
      "............"      
   ]

let data = EmbeddedResource.loadText "Data/Day08.txt"

type Antenna =
   { Frequency : char
     Position : Vector2i }

let allAntennas =
   data
   |> Seq.collecti (fun r ->
      Seq.choosei (fun c ->
         function
         | '.' -> None
         | frequency -> Some { Frequency = frequency; Position = { R = r; C = c } }))

let withinMap (v : Vector2i) =
   v.R >= 0 && v.R < (data |> Seq.length) &&
   v.C >= 0 && v.C < (data |> Seq.head |> Seq.length)

let countAntinodes generateAntinodes =
   allAntennas
   |> Seq.groupBy _.Frequency
   |> Seq.collect (fun (_, antennas) ->
      let pairs =
         (antennas, antennas)
         ||> Seq.allPairs
         |> Seq.filter (fun (a, b) -> a <> b)
      
      pairs
      |> Seq.collect generateAntinodes)
   |> Seq.distinct
   |> Seq.length

let part1 () =
   countAntinodes (fun (a, b) ->
      let v = b.Position - a.Position
      [ b.Position + v
        a.Position - v ]
      |> Seq.filter withinMap)

let part2 () =
   countAntinodes (fun (a, b) ->
      let v = b.Position - a.Position
      seq {
         yield! Seq.initInfinite (fun ord -> a.Position + ord * v) |> Seq.takeWhile withinMap
         yield! Seq.initInfinite (fun ord -> a.Position - ord * v) |> Seq.takeWhile withinMap
      })

let part1Expected = 261

let part2Expected = 898
