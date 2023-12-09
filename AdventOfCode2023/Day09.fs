/// https://adventofcode.com/2023/day/9
module AdventOfCode2023.Day09

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day09.txt"

let dataSets = data |> Seq.map (String.split " " >> Array.map int64)

let rec extrapolate (acc : int64) (xs : int64[]) =
   if xs |> Array.exists ((<>) 0L)
   then xs |> Array.pairwise |> Array.map (fun (a, b) -> b - a) |> extrapolate (acc + (xs |> Array.last))
   else acc

let part1 () = dataSets |> Seq.map (extrapolate 0L) |> Seq.reduce (+)

let part2 () = dataSets |> Seq.map (Array.rev >> extrapolate 0L) |> Seq.reduce (+)

let part1Expected = 1_868_368_343L

let part2Expected = 1022L
