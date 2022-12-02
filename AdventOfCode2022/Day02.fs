namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/2
module Day02 =
    
    let data = EmbeddedResource.loadText "Data/Day02.txt"
    
    let rounds =
        data
        |> Seq.map (fun str -> let arr = str.ToCharArray() in arr[0], arr[2])
        |> List.ofSeq

    type Shape =
        Rock | Paper | Scissors
        member this.Score = match this with Rock -> 1 | Paper -> 2 | Scissors -> 3

    type RoundResult =
        P1Win | Draw | P2Win
        member this.Score = match this with | P1Win -> 0 | Draw -> 3 | P2Win -> 6
        
    let roundOutcomes =
        [ Rock, Rock, Draw
          Rock, Paper, P2Win
          Rock, Scissors, P1Win
          Paper, Rock, P1Win
          Paper, Paper, Draw
          Paper, Scissors, P2Win
          Scissors, Rock, P2Win
          Scissors, Paper, P1Win
          Scissors, Scissors, Draw ]

    let getResult = roundOutcomes |> List.map (fun (p1, p2, r) -> (p1, p2), r) |> Map.ofSeq |> flip Map.find
            
    let getP2 = roundOutcomes |> Seq.map (fun (p1, p2, r) -> (p1, r), p2) |> Map.ofSeq |> flip Map.find

    let (|Shape|) =
        function
        | 'A' | 'X' -> Rock
        | 'B' | 'Y' -> Paper
        | 'C' | 'Z' -> Scissors
        | junk -> failwith $"malformed input: {junk}"
        
    let (|Result|) =
        function
        | 'X' -> P1Win
        | 'Y' -> Draw
        | 'Z' -> P2Win
        | junk -> failwith $"malformed input: {junk}"
    
    let part1 () =
        rounds
        |> Seq.sumBy (fun (Shape p1, Shape p2) ->
            let r = getResult (p1, p2)
            p2.Score + r.Score) 
        |> printfn "%d"

    let part2 () =
        rounds
        |> Seq.sumBy (fun (Shape p1, Result r) ->
            let p2 = getP2 (p1, r)
            p2.Score + r.Score)
        |> printfn "%d"
