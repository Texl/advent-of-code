/// https://adventofcode.com/2023/day/2
module AdventOfCode2023.Day02

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day02.txt"

type Color =
   | Red = 0
   | Green = 1
   | Blue = 2
   
let allColors = [| Color.Red; Color.Green; Color.Blue |] 

type Game =
   { Id : int
     DrawSet : Map<Color, int> array }

let parseGame str =
   let parseDraw str =
      str
      |> String.split ","
      |> Seq.map (function
         | Regex "(\d+) (\w+)" [ Int n; EnumI color ] -> color, n
         | _ -> raise invalidInput)
      |> Map.ofSeq

   let parseDrawSet str =
      match str |> Regex.tryMatch "((?<DrawSet>[^;]+);?)+" with
      | Some matches -> 
         matches["DrawSet"]
         |> Seq.map (fun capture -> parseDraw capture.Value)
         |> Array.ofSeq
      | None -> raise invalidInput

   match str with
   | Regex "Game (\d+): (.*)" [ Int gameId; drawSetStr ] ->
      { Id = gameId
        DrawSet = parseDrawSet drawSetStr }
   | _ -> raise invalidInput

let getMaxColors (game : Game) =
   let all =
      game.DrawSet
      |> Seq.collect Map.toSeq
      |> Seq.groupBy fst
      |> Seq.map (mapSnd (Seq.map snd >> Seq.max)) 
      |> Map.ofSeq

   allColors
   |> Array.map (fun color ->
      all
      |> Map.tryFind color
      |> Option.defaultValue 0)
   
let part1 () =
   data
   |> Seq.map parseGame
   |> Seq.filter (fun game -> (getMaxColors game, [ 12; 13; 14 ]) ||> Seq.forall2 (<=))
   |> Seq.sumBy (fun game -> game.Id)

let part2 () =
   data
   |> Seq.map parseGame
   |> Seq.map (getMaxColors >> Seq.reduce (*))
   |> Seq.sum
   
let part1Expected = 2006

let part2Expected = 84911
