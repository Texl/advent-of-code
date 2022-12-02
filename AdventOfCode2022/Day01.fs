namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/1
module Day01 =
    
    let data = EmbeddedResource.loadText "Data/Day01.txt"

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
