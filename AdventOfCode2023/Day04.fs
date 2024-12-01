/// https://adventofcode.com/2023/day/4
module AdventOfCode2023.Day04

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day04.txt"

type Card =
   { Number : int
     WinningNumbers : Set<int>
     HaveNumbers : Set<int> }

let cards =
   data
   |> Seq.map (fun line ->
      match line |> Regex.tryMatch "Card\s*(?<CardNumber>\d+):(\s*(?<WinningNumber>\d+))+ \|(\s*(?<HaveNumber>\d+))+" with
      | Some m ->
         { Number = m["CardNumber"] |> Seq.exactlyOne |> (fun c -> c.Value |> int)
           WinningNumbers = m["WinningNumber"] |> Seq.map (fun c -> c.Value |> int) |> Set.ofSeq
           HaveNumbers = m["HaveNumber"] |> Seq.map  (fun c -> c.Value |> int) |> Set.ofSeq }
      | None -> raise invalidInput)
   |> List.ofSeq

let part1 () =
   cards
   |> Seq.sumBy (fun card ->
      let numWinningNumbers =
         (card.WinningNumbers, card.HaveNumbers)
         ||> Set.intersect
         |> Set.count
      pown 2 (numWinningNumbers - 1))

let part2 () =
   // cards indexed by card number
   let cardNumberToCard =
      cards
      |> Seq.map (fun card -> card.Number, card)
      |> Map.ofSeq
   
   let cardNumberToWonCards =
      cards
      |> Seq.map (fun card ->
         let numWinningNumbers =
            (card.WinningNumbers, card.HaveNumbers)
            ||> Set.intersect
            |> Set.count

         let wonCardNumbers =
            Array.init numWinningNumbers (fun ord -> card.Number + ord + 1)

         let wonCards =
            wonCardNumbers
            |> Seq.choose (flip Map.tryFind cardNumberToCard)
            |> List.ofSeq

         card.Number, wonCards)
      |> Map.ofSeq
      
   let mutable memoized = Map.empty

   let memoize key f =
      memoized
      |> Map.tryFind key
      |> Option.defaultWith (fun () ->
         let v = f ()
         memoized <- memoized |> Map.add key v
         v)
      
   let getTotalCardsWon card =
      let rec getTotalCardsWonRec card =
         memoize card (fun () ->
            let cardsWon = cardNumberToWonCards |> Map.find card.Number
            cardsWon.Length + (cardsWon |> List.sumBy getTotalCardsWonRec))
        
      getTotalCardsWonRec card
   
   let wonCards =
      cards
      |> List.rev
      |> List.sumBy getTotalCardsWon

   // total cards = original set of cards + won cards
   cards.Length + wonCards

let part1Expected = 21485

let part2Expected = 11024379
