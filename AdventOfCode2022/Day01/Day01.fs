namespace AdventOfCode2022

open AdventOfCode.Common

[<RequireQualifiedAccess>]
module List =
    let split pred xs =
        let f elt (acc, buf) =
            match pred elt, buf with
            | false, buf -> acc, elt :: buf
            | true, [] -> acc, []
            | true, buf -> buf :: acc, []
            
        match (xs, ([], [])) ||> Seq.foldBack f with
        | acc, [] -> acc
        | acc, buf -> buf :: acc


module Day01 =
    
    let data = Utils.loadTextResource "Data/Day01.txt"

    let part1 () =

        let elfInventories =
            data
            |> List.ofSeq
            |> List.split (fun s -> s = "")
            |> List.map (List.map int)
        
        let index, biggest =
            elfInventories
            |> List.indexed
            |> List.maxBy (snd >> List.sum) 

        printfn $"elf index {index} has the biggest total: {biggest |> List.sum} (%A{biggest})"
