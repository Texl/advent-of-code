namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/4
module Day04 =

    let data = EmbeddedResource.loadText "Data/Day04.txt"

    type Circle =
        { Center : int
          Radius : int }
    
    let circlePairs =
        data
        |> Seq.map (function
            | Regex "(\d*)-(\d*),(\d*)-(\d*)" [ I32 x1; I32 x2; I32 y1; I32 y2 ] ->
                // scaled 2x
                { Center = x1 + x2; Radius = x2 - x1 },
                { Center = y1 + y2; Radius = y2 - y1 } 
            | junk -> failwith $"malformed input {junk}")
            
    let part1 () =
        circlePairs
        |> Seq.where (fun (a, b) -> abs (b.Center - a.Center) <= abs (b.Radius - a.Radius))
        |> Seq.length
        |> printfn "%d"

    let part2 () =
        circlePairs
        |> Seq.where (fun (a, b) -> abs (b.Center - a.Center) <= (a.Radius + b.Radius))
        |> Seq.length
        |> printfn "%d"
