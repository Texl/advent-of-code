/// https://adventofcode.com/2024/day/13
module AdventOfCode2024.Day13

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day13.txt"

let testData =
   seq {
      "Button A: X+94, Y+34"
      "Button B: X+22, Y+67"
      "Prize: X=8400, Y=5400"
      ""
      "Button A: X+26, Y+66"
      "Button B: X+67, Y+21"
      "Prize: X=12748, Y=12176"
      ""
      "Button A: X+17, Y+86"
      "Button B: X+84, Y+37"
      "Prize: X=7870, Y=6450"
      ""
      "Button A: X+69, Y+23"
      "Button B: X+27, Y+71"
      "Prize: X=18641, Y=10279"      
   }  

type System =
   { Ax : int64; Ay : int64
     Bx : int64; By : int64
     X  : int64; Y  : int64 }

let systems =
   data
   |> Seq.toList
   |> List.split String.isEmpty
   |> List.map (function
      | [ Regex "Button A: X\+(\d+), Y\+(\d+)" [ Int64 ax; Int64 ay ]
          Regex "Button B: X\+(\d+), Y\+(\d+)" [ Int64 bx; Int64 by ]
          Regex "Prize: X=(\d+), Y=(\d+)"      [ Int64 x;  Int64 y  ] ] ->
         { Ax = ax; Ay = ay
           Bx = bx; By = by
           X  = x;  Y  = y }
      | _ -> raise invalidInput)

let trySolve { Ax = ax; Ay = ay
               Bx = bx; By = by
               X  = x;  Y  = y } =
   let tryIntDivide a b = if a % b = 0L then Some (a / b) else None

   opt {
      let! a = tryIntDivide (by * x - bx * y) (ax * by - ay * bx)
      let! b = tryIntDivide (x - ax * a) bx
      return a, b
   }

let part1 () =
   systems
   |> List.choose trySolve
   |> List.sumBy (fun (a, b) -> 3L * a + b)

let part2 () =
   systems
   |> List.map (fun system -> { system with X = system.X + 10000000000000L
                                            Y = system.Y + 10000000000000L }) 
   |> List.choose trySolve
   |> List.sumBy (fun (a, b) -> 3L * a + b)

let part1Expected = 31623L

let part2Expected = 93209116744825L
