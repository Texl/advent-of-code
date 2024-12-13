/// https://adventofcode.com/2024/day/12
module AdventOfCode2024.Day12

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day12.txt"

let testData =
   [
      "AAAA"
      "BBCD"
      "BBCC"
      "EEEC"

      // "OOOOO"
      // "OXOXO"
      // "OOOOO"
      // "OXOXO"
      // "OOOOO"
      
      // "RRRRIICCFF"
      // "RRRRIICCCF"
      // "VVRRRCCFFF"
      // "VVRCCCJFFF"
      // "VVVVCJJCFE"
      // "VVIVCCJJEE"
      // "VVIIICJJEE"
      // "MIIIIIJJEE"
      // "MIIISIJEEE"
      // "MMMISSJEEE"      
   ]

let map =
   data
   |> Seq.map (Seq.map string >> Seq.toList)
   |> Seq.toList

let neighbors (position : Vector2i) =
   seq {
      vec2i (position.R + 1) position.C
      vec2i (position.R - 1) position.C
      vec2i position.R (position.C + 1)
      vec2i position.R (position.C - 1)
   }
   
let withinMap =
   let rows = map.Length
   let cols = map.Head.Length
   fun (v : Vector2i) ->
      v.R >= 0 && v.R < rows &&
      v.C >= 0 && v.C < cols
   
let valueAt (position : Vector2i) =
   map[position.R][position.C]

let tryValueAt (position : Vector2i) =
   if withinMap position
   then Some (valueAt position)
   else None

let findRegions () =
   let mutable visited : Set<Vector2i> = Set.empty
   
   let rec flood frontier flooded =
      visited <- visited + frontier

      let newFlooded =
         frontier
         |> Seq.collect (fun plot ->
            let plotSym = valueAt plot
            plot
            |> neighbors
            |> Seq.filter (fun neighbor ->
               neighbor |> withinMap &&
               
               not (visited.Contains neighbor) &&
               valueAt neighbor = plotSym))
         |> Set.ofSeq
         
      let newNeighbors = newFlooded - visited
         
      if newNeighbors.IsEmpty
      then flooded
      else flood newNeighbors (newNeighbors + flooded)
   
   let getNextStart () =
      map
      |> Seq.collecti (fun r -> Seq.mapi (fun c cell -> (cell, vec2i r c)))
      |> Seq.filter (fun p -> visited |> Set.contains (snd p) |> not)
      |> Seq.tryHead
   
   let rec _findRegions maybeStart regions =
      match maybeStart with
      | None -> regions
      | Some (sym, plot) ->
         let regionStart = plot |> Set.singleton
         let flooded = flood regionStart regionStart
         _findRegions (getNextStart ()) ((sym, flooded) :: regions)
         
   _findRegions (getNextStart ()) []
      
let perimeter (plots : Set<Vector2i>) =
   plots
   |> Seq.sumBy (fun plot ->
      let adjacents = plot |> neighbors |> Seq.filter plots.Contains |> Seq.length
      4 - adjacents)

let sides (sym : string) (plots : Set<Vector2i>) =
   plots
   |> Seq.sumBy (fun (pos : Vector2i) ->
      let n  = vec2i (pos.R - 1) (pos.C    )
      let ne = vec2i (pos.R - 1) (pos.C + 1)
      let e  = vec2i (pos.R    ) (pos.C + 1)
      let es = vec2i (pos.R + 1) (pos.C + 1)
      let s  = vec2i (pos.R + 1) (pos.C    )
      let sw = vec2i (pos.R + 1) (pos.C - 1)
      let w  = vec2i (pos.R    ) (pos.C - 1)
      let wn = vec2i (pos.R - 1) (pos.C - 1)

      seq {
         n, e, ne
         e, s, es
         s, w, sw
         w, n, wn
      }
      |> Seq.sumBy (fun (side1, side2, corner) ->
         let plotSym = Some sym
         let side1Sym = tryValueAt side1
         let side2Sym = tryValueAt side2
         let cornerSym = tryValueAt corner
         if side1Sym <> plotSym && side2Sym <> plotSym ||
            side1Sym = plotSym && side2Sym = plotSym && cornerSym <> plotSym
         then 1
         else 0))
      

let part1 () = findRegions () |> Seq.sumBy (fun (_, plots) -> plots.Count * (perimeter plots))

let part2 () = findRegions () |> Seq.sumBy (fun (sym, plots) -> plots.Count * (sides sym plots)) 

let part1Expected = 1415378

let part2Expected = 862714
