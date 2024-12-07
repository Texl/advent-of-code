/// https://adventofcode.com/2024/day/7
module AdventOfCode2024.Day07

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day07.txt"

let testData =
   [
      "190: 10 19"
      "3267: 81 40 27"
      "83: 17 5"
      "156: 15 6"
      "7290: 6 8 6 15"
      "161011: 16 10 13"
      "192: 17 8 14"
      "21037: 9 7 18 13"
      "292: 11 6 16 20"      
   ]

type Entry =
   { Result : int64
     Operands : int64 list }

let entries =
   data
   |> Seq.map (function
      | Regex "(\d+): (.*)" [ Int64 result; operands ] ->
         { Result = result
           Operands = operands |> (String.split " " >> Array.map int64 >> Array.toList) }
      | _ -> raise invalidInput)

let test allowConcat entry =
   let rec _test acc result remaining : bool =
      if acc > result then
         false
      else
         match remaining with
         | [] -> acc = result
         | next :: nextRemaining ->
            _test (acc + next) result nextRemaining ||
            _test (acc * next) result nextRemaining ||
            (if allowConcat then _test (int64 $"{acc}{next}") result nextRemaining else false)
            
   _test 0L entry.Result entry.Operands

let part1 () = entries |> Seq.filter (test false) |> Seq.sumBy _.Result


let part2 () = entries |> Seq.filter (test true) |> Seq.sumBy _.Result

let part1Expected = 267566105056L

let part2Expected = 116094961956019L
