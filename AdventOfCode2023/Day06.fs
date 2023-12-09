/// https://adventofcode.com/2023/day/6
module AdventOfCode2023.Day06

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day06.txt"

type Race =
   { Time : int64
     Distance : int64 }

let parse line = line |> String.split " " |> Seq.skip 1 |> Seq.map int64

let parseConcatenated line = line |> String.split " " |> Seq.skip 1 |> String.concat "" |> int64 |> Seq.singleton

let races1 =
   match (data |> List.ofSeq) with
   | [line1; line2] -> (parse line1, parse line2) ||> Seq.zip
   | _ -> raise invalidInput 

let races2 =
   match (data |> List.ofSeq) with
   | [line1; line2] -> (parseConcatenated line1, parseConcatenated line2) ||> Seq.zip
   | _ -> raise invalidInput 

let countWays races =
   races
   |> Seq.map (fun (time, distance) ->
      Array.init (int time) id
      |> Array.filter (fun holdTime -> (time - (int64 holdTime)) * int64 holdTime > distance)
      |> Seq.length)
   |> Seq.reduce (*)

let part1 () =
   races1
   |> countWays
      
let part2 () =
   races2
   |> countWays

let part1Expected = 440000
            
let part2Expected = 26187338
