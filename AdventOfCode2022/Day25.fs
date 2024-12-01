namespace AdventOfCode2022

open System
open AdventOfCode.Common

/// https://adventofcode.com/2022/day/25
module Day25 =
    let data = EmbeddedResource.loadText "Data/Day25.txt"

    let snafuCharToValue =
        function
        | '2' -> 2L
        | '1' -> 1L
        | '0' -> 0L
        | '-' -> -1L
        | '=' -> -2L
        | _ -> raise invalidInput

    let valueToSnafuChar =
        function
        | 2L -> "2"
        | 1L -> "1"
        | 0L -> "0"
        | -1L -> "-"
        | -2L -> "="
        | _ -> raise invalidInput
    
    let snafuToDecimal (snafuValue : string) : int64 =
        (0L, snafuValue)
        ||> Seq.fold (fun v ch -> v * 5L + (snafuCharToValue ch))
    
    let decimalToSnafu (decimalValue : int64) : string =
        decimalValue
        |> List.unfold (function
            | 0L -> None
            | v -> let d = (v + 2L) % 5L - 2L in Some (valueToSnafuChar d, (v - d) / 5L))
        |> List.rev
        |> String.concat ""

    let part1 () =
        data
        |> Seq.map snafuToDecimal
        |> Seq.sum
        |> decimalToSnafu

    let part1Expected = "2--2-0=--0--100-=210"
