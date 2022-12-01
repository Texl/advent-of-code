namespace AdventOfCode2022

open AdventOfCode.Common

[<AutoOpen>]
module Pervasives =
    let mapFst f (a, b) = f a, b
    let mapSnd f (a, b) = a, f b


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

    let elfInventories =
        data
        |> List.ofSeq
        |> List.split (fun s -> s = "")
        |> List.map (List.map int)
        |> List.mapi (fun ord inventory ->
            {| Index = ord
               Inventory = inventory
               Sum = inventory |> List.sum |})
        
    let part1 () =

        let top1 =
            elfInventories
            |> List.maxBy (fun elt -> elt.Sum) 

        printfn $"elf {top1.Index} is carrying the most - {top1.Sum} calories (%A{top1.Inventory})"

    let part2 () =

        let top3 =
            elfInventories
            |> List.sortByDescending (fun elt -> elt.Sum)
            |> List.truncate 3

        let top3Sum = top3 |> List.sumBy (fun elt -> elt.Sum)

        printfn "top3:"
        top3
        |> List.iter (fun elt ->
            printfn $"   elf %3d{elt.Index} is carrying %5d{elt.Sum} calories (%A{elt.Inventory})")
        printfn ""
        printfn $"top3 is carrying {top3Sum} calories in total."
