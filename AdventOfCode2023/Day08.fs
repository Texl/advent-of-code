/// https://adventofcode.com/2023/day/8
module AdventOfCode2023.Day08

open System.Collections.Generic
open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day08.txt"

type Direction =
   | Left
   | Right

let directions =
   data
   |> Seq.head
   |> Seq.map (function
      | 'L' -> Direction.Left
      | 'R' -> Direction.Right
      | _ -> raise invalidInput)
   |> (fun dirs -> Seq.initInfinite (fun _ -> dirs) |> Seq.collect id)

let graph =
   data
   |> Seq.skip 2
   |> Seq.choose (function
      | Regex "(\w+) = \((\w+), (\w+)\)" [ from; left; right ] -> Some(from, (left, right))
      | _ -> raise invalidInput)
   |> Map.ofSeq

let getPathLengthTo isEnd start =
   let rec walk (dirEnumerator : IEnumerator<Direction>) pathLength isEnd last =
      if last |> isEnd then
         pathLength
      else
         dirEnumerator.MoveNext() |> ignore
         let direction = dirEnumerator.Current
         let left, right = graph |> Map.find last
         let next = if direction = Direction.Left then left else right
         walk dirEnumerator (pathLength + 1L) isEnd next

   walk (directions.GetEnumerator()) 0L isEnd start

let part1 () = "AAA" |> getPathLengthTo ((=) "ZZZ")

let part2 () =
   graph.Keys
   |> Seq.filter (String.endsWith "A")
   |> Seq.map (getPathLengthTo (String.endsWith "Z"))
   |> Seq.reduce lcm

let part1Expected = 19_783L

let part2Expected = 9_177_460_370_549L
