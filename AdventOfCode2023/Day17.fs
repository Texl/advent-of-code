/// https://adventofcode.com/2023/day/17
module AdventOfCode2023.Day17

open System
open System.Collections.Generic
open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day17.txt"

type Direction = N = 0 | W = 1 | S = 2 | E = 3
let allDirections = [| Direction.N; Direction.W; Direction.S; Direction.E |]

type Grid(grid : int[][]) =
   static member Parse data =
      data
      |> Seq.map (Seq.map (fun c -> int (c - '0')) >> Array.ofSeq)
      |> Array.ofSeq
      |> Grid
   member this.Height = grid.Length
   member this.Width = grid[0].Length
   member this.Start : Vector2i = { R = 0; C = 0 }
   member this.Goal : Vector2i = { R = this.Height - 1; C = this.Width - 1 }
   member this.Cost(v : Vector2i) = grid[v.R][v.C]

[<Struct>]
type Address = { V : Vector2i; D : Direction; M : int }

[<Struct>]
type GraphNode = { mutable Visited : bool; mutable Distance : int }

let grid : Grid = Grid.Parse data

let solve (minMoves : int) (maxMoves : int) (grid : Grid) =
   let nodes =
      { Visited = false; Distance = Int32.MaxValue }
      |> Array4D.create grid.Height grid.Width 4 (maxMoves + 1)

   let setDistance (a : Address) distance = nodes[a.V.R, a.V.C, int a.D, a.M].Distance <- distance

   let setVisited (a : Address) visited = nodes[a.V.R, a.V.C, int a.D, a.M].Visited <- visited

   let queue = PriorityQueue<Address, int>()

   for dir in allDirections do
      queue.Enqueue({ V = grid.Start; D = dir; M = 0 }, 0)
      setDistance { V = grid.Start; D = dir; M = 0 } 0

   let isVisited (a : Address) = nodes[a.V.R, a.V.C, int a.D, a.M].Visited
   
   let isNotVisited = isVisited >> not

   let getDistance (a : Address) = nodes[a.V.R, a.V.C, int a.D, a.M].Distance

   let getMinDistance v =
      [ for dir in allDirections do
           for moves in 0..maxMoves do
              getDistance { V = v; D = dir; M = moves } ]
      |> Seq.min

   let isValid (a : Address) =
      0 <= a.V.R && a.V.R < grid.Height &&
      0 <= a.V.C && a.V.C < grid.Width &&
      0 <= a.M && a.M <= maxMoves

   let getAdjacent ({ V = v; D = dir; M = moves } : Address) : Address[] =
      let dirs =
         if moves < minMoves then
            [| dir, moves + 1 |]
         else
            match dir with
            | Direction.W | Direction.E -> [| dir, moves + 1; Direction.N, 1; Direction.S, 1 |]
            | Direction.N | Direction.S -> [| dir, moves + 1; Direction.W, 1; Direction.E, 1 |]
            | _ -> failwith "Invalid direction"

      dirs
      |> Array.map (fun (dir, moves) ->
         let delta =
            match dir with
            | Direction.N -> -Vector2i.UnitR
            | Direction.W -> -Vector2i.UnitC
            | Direction.S -> Vector2i.UnitR
            | Direction.E -> Vector2i.UnitC
            | _ -> failwith "Invalid direction"
    
         { V = v + delta
           D = dir
           M = moves })


   let rec visitNext () =
      match queue.TryDequeue() with
      | false, _, _ ->
         getMinDistance grid.Goal
      | true, node, _ when node.V = grid.Goal ->
         getMinDistance grid.Goal
      | true, node, _ when isVisited node ->
         visitNext ()
      | true, node, _ ->
         setVisited node true

         for neighbor in node |> getAdjacent do
            if neighbor |> isValid && neighbor |> isNotVisited then
               let estDist = getDistance node + grid.Cost neighbor.V

               if estDist < getDistance neighbor then
                  setDistance neighbor estDist
                  queue.Enqueue(neighbor, estDist)

         visitNext ()

   visitNext ()

let part1 () = grid |> solve 0 3

let part2 () = grid |> solve 4 10

let part1Expected = 1155

let part2Expected = 1283
