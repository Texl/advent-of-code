/// https://adventofcode.com/2024/day/18
module AdventOfCode2024.Day18

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day18.txt"

let testData =
   seq {
      "5,4"
      "4,2"
      "4,5"
      "3,0"
      "2,1"
      "6,3"
      "2,4"
      "1,5"
      "0,6"
      "3,3"
      "2,6"
      "5,1"
      "1,2"
      "5,5"
      "2,5"
      "6,5"
      "1,4"
      "0,4"
      "6,4"
      "1,1"
      "6,1"
      "1,0"
      "0,5"
      "1,6"
      "2,0"      
   }

let dims = vec2i 71 71

let withinMap (v : Vector2i) =
   v.R >= 0 && v.R < dims.R &&
   v.C >= 0 && v.C < dims.C

let startPosition = vec2i 0 0

let endPosition = vec2i (dims.R - 1) (dims.C - 1)

let allBytes =
   data
   |> Seq.map (function
      | Regex "(\d+),(\d+)" [ Int r; Int c ] -> vec2i r c
      | _ -> raise invalidInput)
   |> Seq.toList

type GridCell = Empty | Corrupted

type Direction = Up | Down | Left | Right

let valueAt (v : Vector2i) (grid : GridCell array array) =
   grid[v.R][v.C]

let move (direction : Direction) (position : Vector2i) =
   match direction with
   | Up -> position - Vector2i.UnitR
   | Down -> position + Vector2i.UnitR
   | Left -> position - Vector2i.UnitC
   | Right -> position + Vector2i.UnitC

let createGrid count =
   let bytesSubset = allBytes |> List.truncate count
   Array.init dims.R (fun r ->
      Array.init dims.C (fun c ->
         let p = vec2i r c
         if bytesSubset |> List.exists ((=) p)
         then Corrupted
         else Empty))

type State =
   { Score : int
     Position : Vector2i }

let search (initialState : State) (count : int) =
   let grid = createGrid count
   
   let mutable bestScore = System.Int32.MaxValue
   let mutable minScores = Map.empty

   let rec solve (queue : Set<State>) =
      if queue.IsEmpty then
         bestScore
      else
         let state = Set.minElement queue
         let remaining = queue |> Set.remove state
 
         if state.Score > bestScore then
            solve remaining
         elif state.Position = endPosition then
            if state.Score < bestScore then
               bestScore <- state.Score
            solve remaining
         else
            let newStates =
               [ state.Position |> move Up 
                 state.Position |> move Down 
                 state.Position |> move Left
                 state.Position |> move Right ]
               |> Seq.choose (fun newPosition ->
                  if not (newPosition |> withinMap) ||
                     grid |> valueAt newPosition = Corrupted then
                     None
                  else
                     let newState =
                        { Position = newPosition
                          Score = state.Score + 1 }
                     
                     let minScore =
                        minScores
                        |> Map.tryFind newState.Position
                        |> Option.defaultValue System.Int32.MaxValue
                     
                     if newState.Score < minScore then
                        minScores <- minScores |> Map.add newState.Position newState.Score
                        Some newState
                     else
                        None)
               |> Set.ofSeq

            solve (remaining + newStates)

   solve (set [ initialState ])

let part1 () = search { Position = startPosition; Score = 0 } 1024

let part2 () =
   let rec searchRec lo hi =
      if lo = hi then
         lo
      elif search { Position = startPosition; Score = 0 } ((hi + lo) / 2) = System.Int32.MaxValue then
         searchRec lo ((hi + lo) / 2 - 1)
      else
         searchRec ((hi + lo) / 2 + 1) hi

   let firstFailure = searchRec 0 allBytes.Length
    
   allBytes |> Seq.truncate firstFailure |> Seq.last

let part1Expected = 356

let part2Expected = vec2i 22 33
