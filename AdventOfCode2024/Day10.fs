/// https://adventofcode.com/2024/day/10
module AdventOfCode2024.Day10

open AdventOfCode.Common

let testData : seq<string> =
   seq {
      "89010123"
      "78121874"
      "87430965"
      "96549874"
      "45678903"
      "32019012"
      "01329801"
      "10456732"      
   }

let data = EmbeddedResource.loadText "Data/Day10.txt"

let map =
   data
   |> Seq.map (Seq.map (string >> int) >> Seq.toArray)
   |> Seq.toArray

let withinMap (v : Vector2i) =
   v.R >= 0 && v.R < (data |> Seq.length) &&
   v.C >= 0 && v.C < (data |> Seq.head |> Seq.length)

let trailheads =
   map
   |> Seq.collecti (fun r ->
      Seq.choosei (fun c cell ->
         if cell = 0 then Some (vec2i r c) else None))
   |> List.ofSeq

let rec scoreTrailhead trailhead =
   let mutable peaks = []
   
   let rec search (v : Vector2i) =
      let cell = map[v.R][v.C]
      
      if cell = 9 then
         peaks <- v :: peaks
      else
         seq {
            vec2i (v.R + 1) v.C
            vec2i (v.R - 1) v.C
            vec2i v.R (v.C + 1)
            vec2i v.R (v.C - 1)
         }
         |> Seq.filter (fun v -> v |> withinMap && map[v.R][v.C] = cell + 1)
         |> Seq.iter search

   search trailhead
   peaks

let part1 () = trailheads |> Seq.sumBy (scoreTrailhead >> Seq.distinct >> Seq.length)

let part2 () = trailheads |> Seq.sumBy (scoreTrailhead >> Seq.length)

let part1Expected = 514

let part2Expected = 1162
