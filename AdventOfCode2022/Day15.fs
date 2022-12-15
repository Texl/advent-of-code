namespace AdventOfCode2022

open AdventOfCode.Common

type Interval =
    { Center : int64
      Radius : int64 }
    member this.Left = this.Center - this.Radius
    member this.Right = this.Center + this.Radius
    static member FromExtents(left, right) =
        { Center = (left + right) / 2L
          Radius = (right - left) / 2L }
    
module Interval =
    let tryUnion (a : Interval) (b : Interval) =
        if abs (b.Center - a.Center) <= a.Radius + b.Radius + 1L
        then Some (Interval.FromExtents(min a.Left b.Left, max a.Right b.Right))
        else None

    let tryIntersection (a : Interval) (b : Interval) =
        let left = max a.Left b.Left
        let right = min a.Right b.Right
        if left <= right
        then Some (Interval.FromExtents(left, right))
        else None
            
    let tryUnions (intervals : Interval list) =
        let sorted = intervals |> List.sortBy (fun interval -> interval.Left)
            
        (([], None), sorted)
        ||> List.fold (fun (acc, prev) inter ->
            match prev with
            | None -> acc, Some inter
            | Some prev ->
                match tryUnion prev inter with
                | Some joined -> acc, Some joined
                | None -> prev :: acc, Some inter)
        |> function
        | acc, Some last -> last :: acc
        | acc, None -> acc


/// https://adventofcode.com/2022/day/15
module Day15 =
    let data = EmbeddedResource.loadText "Data/Day15.txt"

    type Coords = int * int // row, column
    
    type Sensor =
        { Coords : Coords
          NearestBeacon : Coords }
        member this.Distance =
            let (r1, c1), (r2, c2) = this.Coords, this.NearestBeacon
            abs (r2 - r1) + abs (c2 - c1)
            
        member this.TryIntersectRow row =
            let r, c = this.Coords
            let rDist = abs (row - r)
            let cDist = this.Distance - rDist

            if cDist >= 0
            then Some { Center = c; Radius = cDist }
            else None
    
    let sensors =
        let parseSensor (str : string) =
            match str with
            | Regex "Sensor at x=(-*\d+), y=(-*\d+): closest beacon is at x=(-*\d+), y=(-*\d+)" [ Int x; Int y; Int bx; Int by ] ->
                { Coords = y, x
                  NearestBeacon = by, bx }
            | _ -> raise invalidInput 

        data
        |> Seq.map parseSensor
        |> List.ofSeq
    
    let getRowIntervals r possibleRangeLimit =
        let sensorIntersections = sensors |> List.choose (fun sensor -> sensor.TryIntersectRow r)

        let intervals = sensorIntersections |> Interval.tryUnions
        
        let possibleRangeLimit = possibleRangeLimit |> Option.map Interval.FromExtents
        
        match possibleRangeLimit with
        | Some rangeLimit -> intervals |> List.choose (Interval.tryIntersection rangeLimit)
        | None -> intervals
        
    let findBeaconFrequency (maxExtent : int) =
        (fun ord ->
            ord,
            getRowIntervals ord (Some (0, maxExtent))
            |> List.sortBy (fun interval -> interval.Center))
        |> Seq.init maxExtent
        |> Seq.skipWhile (fun (_, overlapping) -> overlapping.Length = 1)
        |> Seq.head
        |> function
            | row, [ a; b ] ->
                let col = (a.Right + b.Left) / 2L
                int64 row + (int64 maxExtent) * int64 col
            | _ -> failwith "Unexpected"
        
    let part1 () =
        let intervals = getRowIntervals 2_000_000 None
        intervals |> Seq.sumBy (fun interval -> interval.Radius * 2L)
        |> printfn "%A"

    let part2 () =
        findBeaconFrequency 4_000_000
        |> printfn "%A"
