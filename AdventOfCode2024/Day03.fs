/// https://adventofcode.com/2024/day/3
module AdventOfCode2024.Day03

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day03.txt"

let testData1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

let testData2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

type Instruction =
   | Mul of int64 * int64
   | Do
   | Dont

let instructions =
   data
   |> Seq.collect (function
      | Regexes "(mul\(\d+,\d+\)|do\(\)|don't\(\))" instructions ->
         instructions
         |> Seq.map (function
            | Regex "mul\((\d+),(\d+)\)" [ Int a; Int b ] -> Mul (a, b)
            | Regex "do\(\)" _ -> Do
            | Regex "don't\(\)" _ -> Dont
            | _ -> failwith "unexpected instruction")
      | _ -> raise invalidInput)
   |> Seq.toList

let part1 () =
   (0L, instructions)
   ||> Seq.fold (fun acc ->
      function
      | Mul (a, b) -> acc + a * b
      | _ -> acc)

let part2 () =
   ((1L, 0L), instructions)
   ||> Seq.fold (fun (mult, acc) ->
      function
      | Mul (a, b) -> mult, acc + mult * a * b
      | Do -> 1L, acc
      | Dont -> 0L, acc)
   |> snd

let part1Expected = 162813399L

let part2Expected = 53783319L
