/// https://adventofcode.com/2024/day/5
module AdventOfCode2024.Day05

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day05.txt"

let testData =
   [
      "47|53"
      "97|13"
      "97|61"
      "97|47"
      "75|29"
      "61|13"
      "75|53"
      "29|13"
      "97|29"
      "53|29"
      "61|53"
      "97|53"
      "61|29"
      "47|13"
      "75|47"
      "97|75"
      "47|61"
      "75|61"
      "47|29"
      "75|13"
      "53|13"
      ""
      "75,47,61,53,29"
      "97,61,53,29,13"
      "75,29,13"
      "75,97,47,61,53"
      "61,13,29"
      "97,13,75,29,47"
   ]

let ruleMap, updates =
   match data |> List.split String.isEmpty with
   | [ rulesData; updatesData ] ->
      rulesData
      |> Seq.map (function
         | Regex "(\d+)\|(\d+)" [ Int a; Int b ] -> a, b
         | _ -> raise invalidInput)
      |> Seq.groupBy fst
      |> Seq.map (mapSnd (Seq.map snd >> Set.ofSeq))
      |> Map.ofSeq,

      updatesData
      |> List.map (String.split "," >> Array.map int)
   | _ -> raise invalidInput

let comp a b =
   match ruleMap |> Map.tryFind a with
   | Some successors -> if successors.Contains(b) then -1 else 1
   | None -> 0

let validUpdates, invalidUpdates =
   updates
   |> List.partition (Seq.pairwise >> Seq.forall (fun (a, b) -> comp a b < 0))

let midpoint (xs : _ array) = xs.[xs.Length / 2] 

let part1 () = validUpdates |> Seq.sumBy midpoint

let part2 () = invalidUpdates |> Seq.sumBy (Array.sortWith comp >> midpoint)

let part1Expected = 5391

let part2Expected = 6142
