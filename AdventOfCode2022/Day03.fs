namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/3
module Day03 =
    
    let data = EmbeddedResource.loadText "Data/Day03.txt"
    
    let rucksacks =
        data
        |> Seq.map (fun str ->
            [ str.Substring(0, str.Length / 2).ToCharArray() |> Set.ofSeq
              str.Substring(str.Length / 2).ToCharArray() |> Set.ofSeq ])
    
    let getPriority : char -> int =
        [ for ord in 1 .. 26 do
            yield char (int 'a' + ord - 1), ord // a-z = 1-26
            yield char (int 'A' + ord - 1), ord + 26 ] // A-Z = 27-52
        |> Map.ofSeq
        |> flip Map.find

    let part1 () =
        rucksacks
        |> Seq.map (fun pockets ->
            pockets
            |> Seq.reduce Set.intersect
            |> Seq.exactlyOne)
        |> Seq.sumBy getPriority
        |> printfn "%d"

    let part2 () =
        rucksacks
        |> Seq.chunkBySize 3
        |> Seq.map (fun rucksacks ->
            rucksacks
            |> Seq.map (Seq.reduce Set.union)
            |> Seq.reduce Set.intersect
            |> Seq.exactlyOne)
        |> Seq.sumBy getPriority
        |> printfn "%d"
