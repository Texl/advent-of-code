namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/6
module Day06 =

    let data = EmbeddedResource.loadText "Data/Day06.txt"
    
    let stream = data |> Seq.exactlyOne |> List.ofSeq

    let rec filter i buffer kernelSize stream =
        match stream with
        | ch :: nextStream -> 
            let nextCounter = i + 1
            let nextBuffer = (ch :: buffer) |> List.truncate kernelSize
            if nextBuffer |> List.distinct |> List.length = kernelSize
            then nextCounter
            else filter nextCounter nextBuffer kernelSize nextStream
        | [] -> failwith "end of stream"

    let part1 () =
        stream
        |> filter 0 [] 4
        |> printfn "%d"

    let part2 () =
        stream
        |> filter 0 [] 14
        |> printfn "%d"
