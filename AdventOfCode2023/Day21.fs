/// https://adventofcode.com/2023/day/21
module AdventOfCode2023.Day21

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day21.txt"

type Tile =
   | GardenPlot of isStart : bool
   | Rock

let grid =
   data
   |> Seq.map (Seq.map (function
      | 'S' -> GardenPlot true
      | '.' -> GardenPlot false
      | '#' -> Rock
      | x -> raiseInvalidInput x)
      >> Array.ofSeq)
   |> Array.ofSeq

let walk grid start =
   let step positions =
      let stepFrom (x, y) = 
         let validStep (x, y) =
            match grid (x, y) with
            | GardenPlot _ -> true
            | _ -> false

         [(x-1, y); (x+1, y); (x, y-1); (x, y+1)] |> Seq.filter validStep

      let p' = positions |> Set.toSeq |> Seq.collect stepFrom |> Set.ofSeq

      Some (p', p')

   Seq.unfold step (Set.singleton start)

let steps s = Seq.skip (s - 1) >> Seq.head

let w, h = grid[0].Length, grid.Length

let part1 () =
   walk (fun (x,y) -> grid[y][x]) (w/2, h/2) |> steps 64 |> Set.count

let part2 () =
   let inputSteps = 26501365L

   let steps = walk (fun (x, y) -> grid[y %% h][x %% w]) (w / 2, h / 2)

   let crossings = Seq.choosei (fun i e -> if i % w = (w / 2 - 1) then Some e else None)

   let c = steps |> crossings |> Seq.take 3 |> Seq.map (Set.count >> int64) |> Seq.toArray

   let n = inputSteps / int64 w

   c[0] + n * (c[1] - c[0]) + (n * (n - 1L) / 2L) * ((c[2] - c[1]) - (c[1] - c[0]))

let part1expected = 3_666

let part2expected = 6_092_987_467_639_52L
