/// https://adventofcode.com/2023/day/22
module AdventOfCode2023.Day22

open AdventOfCode.Common

let testData =
   [
      "1,0,1~1,2,1"
      "0,0,2~2,0,2"
      "0,2,3~2,2,3"
      "0,0,4~0,2,4"
      "2,0,5~2,2,5"
      "0,1,6~2,1,6"
      "1,1,8~1,1,9"
   ]

let data = EmbeddedResource.loadText "Data/Day22.txt"

module Set =
    let isNonEmptySubset s1 s2 = s2 |> Set.isSubset s1 && not s1.IsEmpty 
    
let (|Vector3|_|) str =
   match str |> String.split "," with
   | [| Int x; Int y; Int z |] -> Some { X = x; Y = y; Z= z }
   | _ -> None

type Label = Label of int

type Brick = 
    { Label: Label
      A : Vector3
      B : Vector3 }
    member this.XY = Seq.allPairs [this.A.X .. this.B.X] [this.A.Y .. this.B.Y]
    member this.Height = this.B.Z - this.A.Z
    static member parse ord line =
        match line |> String.split "~" with
        | [| Vector3 a; Vector3 b |] -> { Label = Label ord; A = a; B= b }
        | _ -> raise invalidInput

let stack bricks =
    ((Map.empty, Map.empty), bricks)
    ||> Seq.fold (fun (stackMap, supportMap) (brick: Brick) ->
        let droppedZ =
            brick.XY
            |> Seq.map (fun xy ->
                match stackMap |> Map.tryFind xy with
                | Some (z, _) -> z
                | None -> 0L)
            |> Seq.max
            |> (+) 1L
        
        let supportingBricks =
            brick.XY
            |> Seq.choose (fun xy -> stackMap |> Map.tryFind xy)
            |> Seq.filter (fun (height, _) -> height = droppedZ - 1L)
            |> Seq.map snd
            |> Set.ofSeq
        
        let newStackMap =
            (stackMap, brick.XY)
            ||> Seq.fold (fun s xy ->
                s
                |> Map.add xy (droppedZ + brick.Height, brick.Label))

        let newSupportMap =
            supportMap
            |> Map.add brick.Label supportingBricks 
        
        newStackMap, newSupportMap)

let supports = Seq.mapi Brick.parse >> Seq.sortBy (_.A.Z) >> stack >> snd

let singles = Map.values >> Seq.choose Seq.tryExactlyOne >> Set.ofSeq

let applyBoth f g x = (f x, g x)

let part1' = applyBoth (singles >> Set.count) Seq.length >> fun (a, b) -> b - a

let part2' supports =
    let supportsOf l = supports |> Map.tryFind l |> Option.defaultValue Set.empty
    let rec willFallWithout ls =
        let stillStanding = Map.keys >> Set.ofSeq >> flip (-) ls
        let nextToFall = stillStanding >> Set.filter (fun l -> ls |> Set.isNonEmptySubset (supportsOf l))
        match nextToFall supports with
        | s when Set.isEmpty s -> (Set.count ls) - 1
        | s -> willFallWithout (ls + s) 
    supports |> singles |> Seq.sumBy (Set.singleton >> willFallWithout)

let part1 () =
    data |> supports |> part1'

let part2 () =
    data |> supports |> part2'

let part1Expected = 437

let part2Expected = 42561
