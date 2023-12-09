/// https://adventofcode.com/2023/day/7
module AdventOfCode2023.Day07

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day07.txt"

type Card =
   | Joker = -1
   | Two = 0
   | Three = 1
   | Four = 2
   | Five = 3
   | Six = 4
   | Seven = 5
   | Eight = 6
   | Nine = 7
   | Ten = 8
   | Jack = 9
   | Queen = 10
   | King = 11
   | Ace = 12
   
let parseCard str useJoker =
   match str with
   | "2" -> Card.Two
   | "3" -> Card.Three
   | "4" -> Card.Four
   | "5" -> Card.Five
   | "6" -> Card.Six
   | "7" -> Card.Seven
   | "8" -> Card.Eight
   | "9" -> Card.Nine
   | "T" -> Card.Ten
   | "J" -> if useJoker then Card.Joker else Card.Jack
   | "Q" -> Card.Queen
   | "K" -> Card.King
   | "A" -> Card.Ace
   | _ -> raise invalidInput

type Hand =
   | HighCard = 0
   | OnePair = 1
   | TwoPair = 2
   | ThreeOfAKind = 3
   | FullHouse = 4
   | FourOfAKind = 5
   | FiveOfAKind = 6

let handData =
   data
   |> Seq.map (fun line ->
      match line |> Regex.tryMatch "((?<Card>\d|T|J|Q|K|A)*) (?<Bid>\d*)" with
      | Some m ->
         {| Hand = m["Card"] |> Seq.map (fun c -> parseCard c.Value false) |> List.ofSeq
            JokerHand = m["Card"] |> Seq.map (fun c -> parseCard c.Value true) |> List.ofSeq
            Bid = m["Bid"] |> Seq.map (fun c -> int c.Value) |> Seq.exactlyOne |}
      | _ -> raise invalidInput)
   |> List.ofSeq

let classify (hand : Card list) =
   match hand |> List.countBy id |> List.map snd |> List.sortDescending with
   | [ 5 ] -> Hand.FiveOfAKind
   | 4 :: [ 1 ] -> Hand.FourOfAKind
   | 3 :: [ 2 ] -> Hand.FullHouse
   | 3 :: _ -> Hand.ThreeOfAKind
   | 2 :: 2 :: _ -> Hand.TwoPair
   | 2 :: _ -> Hand.OnePair
   | _ -> Hand.HighCard

let bestJokerHand hand =
   let targets =
      hand
      |> Seq.filter (fun c -> c <> Card.Joker)
      |> Seq.distinct
      |> List.ofSeq
      
   if targets.IsEmpty then
      hand
   else 
      targets
      |> List.map (fun target ->
         hand
         |> List.map (fun card -> if card = Card.Joker then target else card))
      |> List.maxBy classify

let part1 () =
   handData
   |> Seq.sortBy (fun elt -> elt.Hand |> classify, elt.Hand)
   |> Seq.indexed
   |> Seq.sumBy (fun (ord, elt) -> (ord + 1) * elt.Bid)

let part2 () =
   handData
   |> Seq.sortBy (fun elt -> elt.JokerHand |> bestJokerHand |> classify, elt.JokerHand)
   |> Seq.indexed
   |> Seq.sumBy (fun (ord, elt) -> (ord + 1) * elt.Bid)

let part1Expected = 246409899

let part2Expected = 244848487
