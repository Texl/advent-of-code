/// https://adventofcode.com/2023/day/14
module AdventOfCode2023.Day14

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day14.txt"

type Direction = N | W | S | E
    with
    static member All = getAllUnionCases ()

type Tile = Empty | RoundedRock | CubeRock
    with
    static member ToString t =
        match t with
        | CubeRock -> "#"
        | RoundedRock -> "O"
        | Empty -> "."
    static member Parse c =
        match c with
        | '#' -> CubeRock
        | 'O' -> RoundedRock
        | '.' -> Empty
        | x -> raiseInvalidInput x

let grid =
    data
    |> Seq.map (Seq.map Tile.Parse >> List.ofSeq)
    |> List.ofSeq

let measureNLoad grid =
    grid
    |> List.rev
    |> List.transpose
    |> List.sumBy (fun line ->
        line
        |> Seq.mapi (fun i ->
            function
            | RoundedRock -> i + 1
            | _ -> 0)
        |> Seq.sum)

let rotate dir =
    match dir with
    | N -> List.rev >> List.transpose
    | W -> List.map List.rev
    | S -> List.transpose
    | E -> id

let invRotate dir =
    match dir with
    | N -> List.transpose >> List.rev
    | W -> List.map List.rev
    | S -> List.transpose
    | E -> id

let tiltGrid direction grid =
    grid
    |> rotate direction
    |> List.map (fun line ->
        line
        |> Seq.chunkBy ((=) CubeRock)
        |> Seq.collect (snd >> List.sort)
        |> List.ofSeq)
    |> invRotate direction

let cycle =
    Direction.All
    |> Seq.map tiltGrid
    |> Seq.reduce (>>)
    
let rec runToCycle (accumR : _ list) grid =
    let next = cycle grid
    match accumR |> List.tryFindIndex ((=) next) with
    | Some idx ->
        accumR.Length - idx, accumR |> List.take idx |> List.rev
    | None ->
        runToCycle (next :: accumR) next

let findNthCycle n grid =
    let startIdx, cycle = runToCycle [] grid

    let index = n - int64 startIdx - 1L
    
    cycle[index % (int64 cycle.Length) |> int]

let part1 () = grid |> tiltGrid N |> measureNLoad

let part2 () = grid |> findNthCycle 1_000_000_000L |> measureNLoad

let part1Expected = 112048

let part2Expected = 105606
