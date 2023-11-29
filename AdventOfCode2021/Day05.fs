namespace AdventOfCode2021

open AdventOfCode.Common

// https://adventofcode.com/2021/day/5

module Day05 =

    [<AutoOpen>]
    module Day5Data =

        open System

        let dataFilePath = "Data/Day05.txt"

        type Point =
            { X : int
              Y : int }
        
        type Line =
            { P1 : Point
              P2 : Point }

        let (|Int|_|) (str : string) =
            match Int32.TryParse str with
            | true, v -> Some v
            | false, _ -> None

        let (|Split2|_|) (separator : string) (str : string) =
            match str.Split(separator, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray with
            | a :: [ b ] -> Some (a, b)
            | _ -> None
                
        let (|Point|_|) (str : string) =
            match str with
            | Split2 "," (Int x, Int y) -> Some { X = x; Y = y }
            | _ -> None
        
        let parseLine (str : string) =
            match str with
            | Split2 "->" (Point p1, Point p2) -> { P1 = p1; P2 = p2 }
            | junk -> failwith $"Invalid: %A{junk}"
            
        let lines =
            dataFilePath
            |> EmbeddedResource.loadText
            |> Seq.map parseLine
            |> Array.ofSeq

        let lines2 =
            seq {
                "0,9 -> 5,9"
                "8,0 -> 0,8"
                "9,4 -> 3,4"
                "2,2 -> 2,1"
                "7,0 -> 7,4"
                "6,4 -> 2,0"
                "0,9 -> 2,9"
                "3,4 -> 1,4"
                "0,0 -> 8,8"
                "5,5 -> 8,2"
            }
            |> Seq.map parseLine
            |> Array.ofSeq

//    --- Day 5: Hydrothermal Venture ---
//
//    You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.
//
//    They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:
//
//    0,9 -> 5,9
//    8,0 -> 0,8
//    9,4 -> 3,4
//    2,2 -> 2,1
//    7,0 -> 7,4
//    6,4 -> 2,0
//    0,9 -> 2,9
//    3,4 -> 1,4
//    0,0 -> 8,8
//    5,5 -> 8,2
//
//    Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:
//
//        An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
//        An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
//
//    For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.
//
//    So, the horizontal and vertical lines from the above list would produce the following diagram:
//
//    .......1..
//    ..1....1..
//    ..1....1..
//    .......1..
//    .112111211
//    ..........
//    ..........
//    ..........
//    ..........
//    222111....
//
//    In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.
//
//    To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.
//
//    Consider only horizontal and vertical lines. At how many points do at least two lines overlap?

    let pointsInLine (line : Line) =
        match line.P2.X - line.P1.X, line.P2.Y - line.P1.Y with
        | 0, 0 -> [ line.P1 ]
        | dx, 0 -> [ for x = 0 to abs dx do { X = line.P1.X + x * sign dx; Y = line.P1.Y } ] // horizontal
        | 0, dy -> [ for y = 0 to abs dy do { X = line.P1.X; Y = line.P1.Y + y * sign dy } ] // vertical
        | dx, dy -> [ for x = 0 to abs dx do { X = line.P1.X + x * sign dx; Y = line.P1.Y + x * sign dx * dy / dx } ] // general line - can assume 45 degrees, so integer math is fine
    
    let getPointToOverlap (lines : #seq<Line>) =
        (Map.empty, lines)
        ||> Seq.fold (fun s n ->
            let points = pointsInLine n
            (s, points)
            ||> List.fold (fun s2 n2 ->
                match s2 |> Map.tryFind n2 with
                | Some v -> s2 |> Map.add n2 (v + 1)
                | None -> s2 |> Map.add n2 1))

    let part1 () =
        
        let horizontalsAndVerticals =
            lines
            |> Seq.filter (fun line ->
                match line.P2.X - line.P1.X, line.P2.Y - line.P1.Y with
                | 0, 0
                | _, 0
                | 0, _ -> true
                | _, _ -> false)
        
        let pointToOverlap = getPointToOverlap horizontalsAndVerticals

        let pointsWithOverlap2Plus =
            pointToOverlap
            |> Map.filter (fun _k v -> v > 1)
            |> Map.toSeq
            |> Seq.length
            
        printfn $"{pointsWithOverlap2Plus}"

//    --- Part Two ---
//
//    Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.
//
//    Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:
//
//        An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
//        An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
//
//    Considering all lines from the above example would now produce the following diagram:
//
//    1.1....11.
//    .111...2..
//    ..2.1.111.
//    ...1.2.2..
//    .112313211
//    ...1.2....
//    ..1...1...
//    .1.....1..
//    1.......1.
//    222111....
//
//    You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.
//
//    Consider all of the lines. At how many points do at least two lines overlap?

    let part2 () =
        
        let pointToOverlap = getPointToOverlap lines

        let pointsWithOverlap2Plus =
            pointToOverlap
            |> Map.filter (fun _k v -> v > 1)
            |> Map.toSeq
            |> Seq.length
            
        printfn $"{pointsWithOverlap2Plus}"
