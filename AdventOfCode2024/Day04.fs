/// https://adventofcode.com/2024/day/4
module AdventOfCode2024.Day04

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day04.txt"

let testData =
   [
      "MMMSXXMASM"
      "MSAMXMSMSA"
      "AMXSXMAAMM"
      "MSAMASMSMX"
      "XMASAMXAMM"
      "XXAMMXXAMA"
      "SMSMSASXSS"
      "SAXAMASAAA"
      "MAMMMXMMMM"
      "MXMXAXMASX"   
   ]

let puzzle = data |> Seq.map (Seq.map id >> List.ofSeq) |> List.ofSeq

let part1 () =
   [ puzzle
     puzzle |> List.transpose
     puzzle |> List.diagonals
     puzzle |> List.map List.rev |> List.diagonals ]
   |> List.collect (fun l -> [ l; l |> List.map List.rev ]) |> List.concat
   |> Seq.collect (Seq.tails >> Seq.map (Seq.truncate 4 >> Seq.toList))
   |> Seq.filter (function [ 'X'; 'M'; 'A'; 'S' ] -> true | _ -> false)
   |> Seq.length

let part2 () =
   [
      for r in 1 .. puzzle.Length - 2 do
      for c in 1 .. puzzle[r].Length - 2 do
         if puzzle[r][c] = 'A' then
            let diag1 = puzzle[r - 1][c - 1], puzzle[r + 1][c + 1]
            let diag2 = puzzle[r + 1][c - 1], puzzle[r - 1][c + 1]
            if (diag1 = ('M', 'S') || diag1 = ('S', 'M')) &&
               (diag2 = ('M', 'S') || diag2 = ('S', 'M')) then
               yield 1
            else
               yield 0
   ]
   |> Seq.sum
            
let part1Expected = 2406

let part2Expected = 1807
