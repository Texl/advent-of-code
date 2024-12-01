namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/18
module Day18 =
    let data = EmbeddedResource.loadText "Data/Day18.txt"
    
    let input =
        data
        |> Seq.map (function
            | Regex "(\d+),(\d+),(\d+)" [ Int x; Int y; Int z ] ->
                { X = x
                  Y = y
                  Z = z }
            | _ -> raise invalidInput)
        |> Set.ofSeq

    let getNeighbors (v : Vector3) =
        [ v + Vector3.UnitX
          v - Vector3.UnitX
          v + Vector3.UnitY
          v - Vector3.UnitY
          v + Vector3.UnitZ
          v - Vector3.UnitZ ]
    
    let part1 () =
        input
        |> Seq.sumBy (fun cube ->
            let totalFaces = 6

            let coveredFaces =
                cube
                |> getNeighbors 
                |> Seq.filter input.Contains
                |> Seq.length

            totalFaces - coveredFaces)
        |> printfn "%d"

    let part2 () =
        let min = (input |> Seq.reduce Vector3.minElements) - Vector3.One
        let max = (input |> Seq.reduce Vector3.maxElements) + Vector3.One

        let isInBounds v =
            v.X >= min.X && v.X <= max.X &&
            v.Y >= min.Y && v.Y <= max.Y &&
            v.Z >= min.Z && v.Z <= max.Z

        let rec flood (frontier : Set<Vector3>) (floodedSoFar : Set<Vector3>) =
            let frontierNeighbors = frontier |> Seq.collect getNeighbors |> Seq.filter isInBounds |> Set.ofSeq
            let newlyFlooded = frontierNeighbors - floodedSoFar - input
            if newlyFlooded.IsEmpty
            then floodedSoFar
            else flood newlyFlooded (floodedSoFar + newlyFlooded)
        
        let flooded =
            let start = min |> Set.singleton
            flood start start

        input
        |> Seq.sumBy (fun cube ->
            getNeighbors cube
            |> Seq.filter (fun coords -> flooded |> Set.contains coords)
            |> Seq.length)
        |> printfn "%d"
