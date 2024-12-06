/// https://adventofcode.com/2024/day/6
module AdventOfCode2024.Day06

open System.Collections.Generic
open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day06.txt"

let testData =
   [
      "....#....."
      ".........#"
      ".........."
      "..#......."
      ".......#.."
      ".........."
      ".#..^....."
      "........#."
      "#........."
      "......#..."      
   ]

type Direction = Up | Down | Left | Right

type Tile = Guard | Obstruction | Empty

let tiles =
   data
   |> Seq.map (fun row ->
      row |> Seq.map (function
         | '.' -> Empty
         | '^' -> Guard
         | '#' -> Obstruction
         | _ -> raise invalidInput)
      |> Seq.toArray)
   |> Seq.toArray

let tryTileAt struct (r, c) =
   if r < 0 || r >= tiles.Length ||
      c < 0 || c >= tiles[r].Length
   then ValueNone
   else ValueSome (tiles[r][c])

let move struct (r, c) direction =
   match direction with
   | Up -> struct (r - 1, c)
   | Down -> struct (r + 1, c)
   | Left -> struct (r, c - 1)
   | Right -> struct (r, c + 1)

let turn =
   function
   | Up -> Right
   | Down -> Left
   | Left -> Up
   | Right -> Down

let guardTile =
   tiles
   |> Seq.collecti (fun r row ->
      row |> Seq.mapi (fun c col ->
         if col = Guard then
            Some struct (r, c)
         else
            None))
   |> Seq.choose id
   |> Seq.exactlyOne

let getVisitedTiles () =
   let mutable guardPos = guardTile
   let mutable guardTile = ValueSome Guard
   let mutable guardDir = Up
   let visited = HashSet()
   
   while guardTile.IsValueSome do
      visited.Add(guardPos) |> ignore
      let nextPos = move guardPos guardDir
      match tryTileAt nextPos with
      | ValueSome Obstruction ->
         guardDir <- turn guardDir
      | tile ->
         guardPos <- nextPos
         guardTile <- tile
   
   visited
   
let part1 () =
   let visited = getVisitedTiles ()
   visited.Count

let part2 () =
   let visited = getVisitedTiles ()
      
   let found = HashSet()

   for candidate in visited do
      let tryModifiedTileAt pos =
         if pos = candidate
         then ValueSome Obstruction
         else tryTileAt pos

      let mutable guardPos = guardTile
      let mutable guardDir = Up
      let mutable finished = false
      let seen = HashSet()
      
      while not finished do
         if seen.Contains(struct (guardDir, guardPos)) then
            found.Add(candidate) |> ignore
            finished <- true
         else
            seen.Add(struct (guardDir, guardPos)) |> ignore
            let nextPos = move guardPos guardDir
            match tryModifiedTileAt nextPos with
            | ValueSome Obstruction ->
               guardDir <- turn guardDir
            | tile ->
               guardPos <- nextPos
               finished <- tile.IsNone
            
   found.Count
   
let part1Expected = 4559

let part2Expected = 1604
