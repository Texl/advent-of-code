namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/9
module Day09 =

    let data = EmbeddedResource.loadText "Data/Day09.txt"

    type Direction = Left | Right | Up | Down

    let steps =
        data
        |> Seq.collect (function
            | Regex "L (\d*)" [ Int d ] -> Left |> Seq.replicate d
            | Regex "R (\d*)" [ Int d ] -> Right |> Seq.replicate d
            | Regex "U (\d*)" [ Int d ] -> Up |> Seq.replicate d
            | Regex "D (\d*)" [ Int d ] -> Down |> Seq.replicate d
            | _ -> failwith "unexpected input")
        |> List.ofSeq

    let advanceRope rope step =
        let advanceFirstKnot direction (x, y) =
            match direction with
            | Left -> x - 1, y
            | Right -> x + 1, y
            | Up -> x, y + 1
            | Down -> x, y - 1
            
        let advanceKnot (x2, y2) (x1, y1) =
            let dx = x2 - x1
            let dy = y2 - y1

            if dx * dx + dy * dy > 2
            then x1 + sign dx, y1 + sign dy
            else x1, y1
            
        match rope with
        | first :: rest ->
            let newFirst = first |> advanceFirstKnot step
            (newFirst, rest) ||> List.scan advanceKnot
        | _ -> failwith "err"

    let getNumUniqueTailPositions numKnots steps =
        let initialRopeState = (0, 0) |> List.replicate numKnots
        let ropeStates = (initialRopeState, steps) ||> Seq.scan advanceRope 
        let uniqueTailPositions = ropeStates |> Seq.map List.last |> Seq.distinct |> Seq.length
        uniqueTailPositions
        
    let part1 () =
        let numKnots = 2
        getNumUniqueTailPositions numKnots steps
        |> printfn "%d"

    let part2 () =
        let numKnots = 10
        getNumUniqueTailPositions numKnots steps
        |> printfn "%d"
