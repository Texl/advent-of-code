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

type Graph(height : int, width : int, minMoves : int, maxMoves : int) =
   let numDirections = 4
   let nodes = { Visited = false; Distance = Int32.MaxValue } |> Array4D.create height width numDirections (maxMoves + 1)
   member this.SetDistance(a : Address, distance : int) = nodes[a.V.R, a.V.C, int a.D, a.M].Distance <- distance
   member this.SetVisited(a : Address, visited : bool) = nodes[a.V.R, a.V.C, int a.D, a.M].Visited <- visited
   member this.IsVisited(a : Address) : bool = nodes[a.V.R, a.V.C, int a.D, a.M].Visited
   member this.IsNotVisited(a : Address) : bool = this.IsVisited(a) |> not
   member this.GetDistance(a : Address) : int = nodes[a.V.R, a.V.C, int a.D, a.M].Distance

   member this.IsValid(a : Address) : bool =
      0 <= a.V.R && a.V.R < height &&
      0 <= a.V.C && a.V.C < width &&
      // direction always valid
      0 <= a.M && a.M <= maxMoves
   
   member this.GetAdjacent(a : Address) =
      let { V = v; D = dir; M = moves } = a
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
         { V = v + delta; D = dir; M = moves })

   member this.GetMinDistanceTo(v : Vector2i) =
      [ for dir in allDirections do
           for moves in 0..maxMoves do
              this.GetDistance({ V = v; D = dir; M = moves }) ]
      |> Seq.min

let grid : Grid = Grid.Parse data

let solve (minMoves : int) (maxMoves : int) (grid : Grid) =
   let graph = Graph(grid.Width, grid.Height, minMoves, maxMoves)

   let queue = PriorityQueue<Address, int>()

   for dir in allDirections do
      queue.Enqueue({ V = grid.Start; D = dir; M = 0 }, 0)
      graph.SetDistance({ V = grid.Start; D = dir; M = 0 }, 0)

   let rec visitNext () =
      match queue.TryDequeue() with
      | false, _, _ -> graph.GetMinDistanceTo(grid.Goal)
      | true, node, _ when node.V = grid.Goal -> graph.GetMinDistanceTo(grid.Goal)
      | true, node, _ when graph.IsVisited(node) -> visitNext ()
      | true, node, _ ->
         graph.SetVisited(node, true)

         for neighbor in graph.GetAdjacent(node) do
            if graph.IsValid(neighbor) && graph.IsNotVisited(neighbor) then
               let estDist = graph.GetDistance(node) + grid.Cost(neighbor.V)

               if estDist < graph.GetDistance(neighbor) then
                  graph.SetDistance(neighbor, estDist)
                  queue.Enqueue(neighbor, estDist)

         visitNext ()

   visitNext ()

let part1 () = grid |> solve 0 3

let part2 () = grid |> solve 4 10

let part1Expected = 1155

let part2Expected = 1283
