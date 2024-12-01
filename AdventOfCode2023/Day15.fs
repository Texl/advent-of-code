/// https://adventofcode.com/2023/day/15
module AdventOfCode2023.Day15

open AdventOfCode.Common

let data =
   EmbeddedResource.loadText "Data/Day15.txt"
   |> Seq.exactlyOne
   |> fun s -> s |> String.split ","

let hash str = (0, str) ||> Seq.fold (fun s n -> (s + int n) * 17 % 256)
   
type Op =
   | Remove of label : string * hash : int
   | Replace of label : string * hash : int * focalLength : int
   with
   static member Parse =
      function
      | Regex "(\w+)-" [ label ] -> Remove (label, hash label)
      | Regex "(\w+)=(\d+)" [ label; Int focalLength ] -> Replace (label, hash label, focalLength)
      | _ -> raise invalidInput
   
type Lens =
   { Label : string
     FocalLength : int }
   
type Box =
   { LensesR : Lens list }
   
let part1 () = data |> Seq.sumBy hash

let part2 () =
   let ops = data |> Array.map Op.Parse

   let initialBoxes = List.init 256 (fun i -> { LensesR = [] })

   let finalBoxes =
      (initialBoxes, ops)
      ||> Seq.fold (fun boxes ->
         function
         | Remove (label, hash) ->
            let newBox = { LensesR = boxes[hash].LensesR |> List.filter (fun lens -> lens.Label <> label) }
            boxes |> List.updateAt hash newBox
            
         | Replace (label, hash, focalLength) ->
            let newLens = { Label = label; FocalLength = focalLength }
            let newBox =
               match boxes[hash].LensesR |> List.tryFindIndex (fun lens -> lens.Label = label) with
               | Some index -> { LensesR = (boxes[hash]).LensesR |> List.updateAt index newLens }
               | None -> { LensesR = newLens :: (boxes[hash]).LensesR }
            boxes |> List.updateAt hash newBox)

   finalBoxes
   |> Seq.collecti (fun boxOrd box ->
      box.LensesR
      |> List.rev
      |> Seq.mapi (fun lensOrd lens -> (boxOrd + 1) * (lensOrd + 1) * lens.FocalLength))
   |> Seq.sum

let part1Expected = 517551

let part2Expected = 286097