/// https://adventofcode.com/2023/day/3
module AdventOfCode2023.Day03

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day03.txt"

type Symbol =
   { Symbol : string
     Position : Vector2 }

type PartNumberCandidate =
   { Number : int
     AdjacentBounds : AABB }
   with
   member self.AdjacentTo (symbol : Symbol) =
      self.AdjacentBounds.Contains symbol.Position

let partNumberCandidates, symbols =
   data
   |> Seq.mapi (fun row line ->
      match line |> Regex.tryMatch "(\.+|(?<Candidate>\d+)|(?<Symbol>[^\.\d])){1,}" with
      | Some namedCaptures -> row, namedCaptures
      | None -> raise invalidInput)
   |> Seq.map (fun (row, namedCaptures) ->
      let candidates =
         namedCaptures["Candidate"]
         |> Array.map (fun candidate ->
            let position = { R = row; C = candidate.Index }
            let length = candidate.Value.Length
            let adjacentBounds =
               { Min = position - Vector2.One
                 Max = position + Vector2.One + (length - 1) * Vector2.UnitC }
            { Number = int candidate.Value
              AdjacentBounds = adjacentBounds })

      let symbols =
         namedCaptures["Symbol"]
         |> Array.map (fun candidate ->
            { Symbol = candidate.Value
              Position = { R = row; C = candidate.Index } })
      
      candidates, symbols)
   |> Seq.unzip
   |> fun (candidates, symbols) ->
      candidates |> Array.concat, symbols |> Array.concat

let isPartNumber (candidate : PartNumberCandidate) =
   symbols |> Seq.exists candidate.AdjacentTo

let part1 () =
   partNumberCandidates
   |> Seq.filter isPartNumber
   |> Seq.sumBy (fun candidate -> candidate.Number) 

let tryGetGearRatio (symbol : Symbol) =
   match symbol.Symbol, partNumberCandidates |> Array.filter (fun candidate -> candidate.AdjacentTo symbol) with
   | "*", [| pn1; pn2 |] -> Some (pn1.Number * pn2.Number)
   | _ -> None
   
let part2 () =
   symbols
   |> Seq.choose tryGetGearRatio
   |> Seq.sum

let part1Expected = 538046

let part2Expected = 81709807
