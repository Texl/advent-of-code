namespace AdventOfCode2022

open System
open AdventOfCode.Common

/// https://adventofcode.com/2022/day/5
module Day05 =

    let data = EmbeddedResource.loadText "Data/Day05.txt"

    type Move =
        { Quantity : int
          FromIndex : int
          ToIndex : int }

    let parsedStacks, parsedMoves =
        // reverse lines, transpose, parse stacks
        let parseStacks (lines : string list) =
            lines
            |> List.rev
            |> Seq.map List.ofSeq
            |> List.transpose
            |> Array.ofList
            |> Array.choose (function
                | Int id :: contents -> contents |> List.rev |> List.skipWhile Char.IsWhiteSpace |> Some 
                | _ -> None)
            
        let parseMoves lines =
            lines
            |> List.map (function
                | Regex "move (\d*) from (\d*) to (\d*)" [ Int quantity; Int fromIndex; Int toIndex ] ->
                    { Quantity = quantity
                      FromIndex = fromIndex - 1
                      ToIndex = toIndex - 1 }
                | junk -> failwith $"unexpected input: {junk}")

        data
        |> List.ofSeq
        |> List.split (fun str -> str = "")
        |> function
            | [ stackLines; moveLines ] -> stackLines |> parseStacks, moveLines |> parseMoves
            | junk -> failwith $"unexpected input {junk}"

    let private printStacks stacks =
        stacks
        |> Array.iter (fun xs ->
            xs
            |> List.tryHead
            |> Option.defaultValue '.'
            |> string
            |> printf "%s")
        printfn ""
    
    let part1 () =
        (parsedStacks, parsedMoves)
        ||> Seq.fold (fun stacks move ->
            let toMove, newFrom = stacks[move.FromIndex] |> List.splitAt move.Quantity
            stacks
            |> Array.mapi (fun ord stack ->
                if ord = move.FromIndex then newFrom
                elif ord = move.ToIndex then (toMove |> List.rev) @ stack
                else stack))
        |> printStacks
        printfn "VRWBSFZWM expected"

    let part2 () =
        (parsedStacks, parsedMoves)
        ||> Seq.fold (fun stacks move ->
            let toMove, newFrom = stacks[move.FromIndex] |> List.splitAt move.Quantity
            stacks
            |> Array.mapi (fun ord stack ->
                if ord = move.FromIndex then newFrom
                elif ord = move.ToIndex then toMove @ stack
                else stack))
        |> printStacks
        printfn "RBTWJWMCF expected"
