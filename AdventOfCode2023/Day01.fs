/// https://adventofcode.com/2023/day/1
module AdventOfCode2023.Day01

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day01.txt"

let part1 () =
   let parseCalibrationValue line =
      let digits =
         match line with
         | Regexes "(\d)" nums -> nums |> Seq.map int |> Array.ofSeq
         | _ -> [||]
         
      digits[0] * 10 + digits[^0]
   
   data   
   |> Seq.sumBy parseCalibrationValue

let part2 () =
   let parseCalibrationValue line =
      let digits =
         line
         |> String.tails
         |> Seq.choose (function
            | StartsWith "zero" _ -> Some 0
            | StartsWith "one" _ -> Some 1
            | StartsWith "two" _ -> Some 2
            | StartsWith "three" _ -> Some 3
            | StartsWith "four" _ -> Some 4
            | StartsWith "five" _ -> Some 5
            | StartsWith "six" _ -> Some 6
            | StartsWith "seven" _ -> Some 7
            | StartsWith "eight" _ -> Some 8
            | StartsWith "nine" _ -> Some 9
            | Regex "^(\d)" [ Int n ] -> Some n
            | _ -> None)
         |> Array.ofSeq
      
      digits[0] * 10 + digits[^0]
      
   data
   |> Seq.sumBy parseCalibrationValue

let part1Expected = 54630

let part2Expected = 54770
