namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/2
module Day02 =
    
    let data = EmbeddedResource.loadText "Data/Day02.txt"

    let part1 () =
        data
        |> Seq.sumBy (function
            | "A X" -> 1 + 3
            | "A Y" -> 2 + 6
            | "A Z" -> 3 + 0
            | "B X" -> 1 + 0
            | "B Y" -> 2 + 3
            | "B Z" -> 3 + 6
            | "C X" -> 1 + 6
            | "C Y" -> 2 + 0
            | "C Z" -> 3 + 3
            | junk -> failwith $"malformed input: {junk}")
        |> printfn "%d"

    let part2 () =
        data
        |> Seq.sumBy (function
            | "A X" -> 3 + 0
            | "A Y" -> 1 + 3
            | "A Z" -> 2 + 6
            | "B X" -> 1 + 0
            | "B Y" -> 2 + 3
            | "B Z" -> 3 + 6
            | "C X" -> 2 + 0
            | "C Y" -> 3 + 3
            | "C Z" -> 1 + 6
            | junk -> failwith $"malformed input: {junk}")
        |> printfn "%d"
