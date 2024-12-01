/// https://adventofcode.com/2023/day/16
module AdventOfCode2023.Day16

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day16.txt"

type Tile =
   | Mirror0
   | Mirror45
   | Mirror90
   | Mirror135
   | Empty
   with
   static member Parse c =
      match c with
      | '-' -> Mirror0
      | '/' -> Mirror45
      | '|' -> Mirror90
      | '\\' -> Mirror135
      | '.' -> Empty
      | x -> raiseInvalidInput x

   override this.ToString () =
      match this with
      | Mirror0 -> "-"
      | Mirror45 -> "/"
      | Mirror90 -> "|"
      | Mirror135 -> "\\"
      | Empty -> "."

type Direction = N | W | S | E

let grid =
   data
   |> Seq.map (Seq.map Tile.Parse >> List.ofSeq) |> List.ofSeq 
let rec tick (acc : Set<_>) (grid : Tile list list) (lights : (Direction * Vector2) list) =
   match lights with
   | [] -> acc
   | light :: remLights ->
      let lightDir, lightPos = light
      
      let tile = grid[int lightPos.R][int lightPos.C]
            
      let nextLights =
         seq {
            match lightDir, tile with
            | N, Empty
            | E, Mirror45
            | N, Mirror90
            | W, Mirror135 ->
               yield N, lightPos - Vector2.UnitR

            | S, Empty
            | W, Mirror45
            | S, Mirror90
            | E, Mirror135 ->
               yield S, lightPos + Vector2.UnitR

            | W, Empty
            | W, Mirror0
            | S, Mirror45
            | N, Mirror135 ->
               yield W, lightPos - Vector2.UnitC

            | E, Empty
            | E, Mirror0
            | N, Mirror45
            | S, Mirror135 ->
               yield E, lightPos + Vector2.UnitC

            | (N | S), Mirror0 ->
               yield W, lightPos - Vector2.UnitC
               yield E, lightPos + Vector2.UnitC

            | (W | E), Mirror90 ->
               yield N, lightPos - Vector2.UnitR
               yield S, lightPos + Vector2.UnitR
         }
         |> Seq.filter (fun (_, lightPos) ->
            0L <= lightPos.R && lightPos.R < grid.Length &&
            0L <= lightPos.C && lightPos.C < grid.Head.Length)
         |> List.ofSeq

      let nextAcc, nextLightsR =
         ((acc, []), nextLights)
         ||> Seq.fold (fun (s, l) n ->
            if s |> Set.contains n then
               s, l
            else
               s |> Set.add n, n :: l)
            
      tick nextAcc grid (remLights @ (nextLightsR |> List.rev))

let countEnergized initialLight grid =
   let seen = tick (Set.singleton initialLight) grid [ initialLight ]
   seen |> Seq.map snd |> Seq.distinct |> Seq.length

let part1 () = grid |> countEnergized (E, Vector2.Zero) 

let part2 () =
   let maxRow = grid.Length - 1
   let maxCol = grid.Head.Length - 1
   
   seq {
      for c in 0 .. maxCol do 
        S, { R = 0; C = c }
      for c in 0 .. maxCol do 
        N, { R = int64 maxRow; C = c }
      for r in 0 .. maxRow do 
        E, { R = r; C = 0 }
      for r in 0 .. maxRow do 
        W, { R = r; C = int64 maxCol }
   }
   |> Seq.map (fun start -> grid |> countEnergized start)
   |> Seq.max

let part1Expected = 7623

let part2Expected = 8244
