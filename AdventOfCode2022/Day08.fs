namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/8
module Day08 =

    let data = EmbeddedResource.loadText "Data/Day08.txt"

    type Tree =
        { Coords : int * int
          Height : int }

    // int grid        
    let treeHeightGrid =
        data
        |> Seq.map (Seq.map (string >> int) >> List.ofSeq)
        |> List.ofSeq

    // tree grid
    let treeGrid =
        treeHeightGrid
        |> List.mapi (fun row ->
            List.mapi (fun col height ->
                { Coords = row, col
                  Height = height }))
    
    let getTreesVisibleFromLeft row =
        let highest = row |> List.maxBy (fun tree -> tree.Height)

        let candidates = row |> List.takeWhile (fun tree -> tree.Height < highest.Height)

        let visibleBeforeHighest =
            ([], candidates)
            ||> List.fold (fun (accR : Tree list) next ->
                match accR with
                | last :: _ -> if next.Height > last.Height then next :: accR else accR
                | [] -> [ next ])
            
        (highest :: visibleBeforeHighest) |> List.rev
        
    let cheatDirections (grid : _ list list) =
        seq {
            yield! grid // +col
            yield! grid |> List.map List.rev // -col
            yield! grid |> List.transpose // +row
            yield! grid |> List.map List.rev |> List.transpose // -row
        }
        
    let part1 () =
        treeGrid
        |> cheatDirections
        |> Seq.collect getTreesVisibleFromLeft
        |> Seq.distinctBy (fun tree -> tree.Coords)
        |> Seq.length
        |> printfn "%d"

    let part2 () =

        let getVisibleToRight row = 
            row
            |> Seq.tails
            |> Seq.map List.ofSeq 
            |> Seq.choose (function
                | currentTree :: toRight ->
                    let visible =
                        toRight
                        |> List.takeWhile (fun t -> t.Height < currentTree.Height)
                        |> List.length

                    let atEnd = visible = toRight.Length
                    Some (currentTree.Coords, if atEnd then visible else visible + 1)
                | [] -> None)
            |> List.ofSeq

        treeGrid
        |> cheatDirections
        |> Seq.collect getVisibleToRight
        |> Seq.groupBy fst
        |> Seq.map (fun (k, g) -> (k, g |> Seq.map snd |> Seq.reduce (*)))
        |> Seq.maxBy snd
        |> printfn "%A"
