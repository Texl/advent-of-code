/// https://adventofcode.com/2023/day/12
module AdventOfCode2023.Day12

open AdventOfCode.Common

open System.Collections.Generic

let data = EmbeddedResource.loadText "Data/Day12.txt"

type Condition =
   | Operational
   | Damaged
   | Unknown
   with
   static member ofChar =
      function
      | '.' -> Operational
      | '#' -> Damaged
      | '?' -> Unknown
      | _ -> raise invalidInput 

let entries =
   data
   |> Seq.map (fun line ->
      match line |> String.split " " with
      | [| conditions; runs |] ->
         conditions |> Seq.map Condition.ofChar |> List.ofSeq,
         runs |> String.split "," |> Seq.map int |> List.ofSeq
      | _ -> raise invalidInput)

let memoize =
   let cache = Dictionary<_,_>()
   fun key f ->
      cache
      |> Dict.tryFind key
      |> Option.defaultWith (fun () ->
         let v = f ()
         cache[key] <- v
         v)

type State =
   | Idle
   | Continuing of int
   | Ending

let count (initialConditions : Condition list, initialGroups : int list) =
   
   let rec countRec (state : State) (conditions : Condition list) (groups : int list) =
     
      let key = state, conditions, groups

      let advance =
         function
         | 1 -> Ending
         | n -> Continuing (n - 1)
      
      memoize key (fun () ->
         match key with
         // Ended -> Ended
         | Idle, Unknown :: cs, [] -> countRec Idle cs []
         | Idle, Operational :: cs, rs -> countRec Idle cs rs
         // Ended -> (Continuing | Ending)
         | Idle, Damaged :: cs, r :: rt -> countRec (advance r) cs rt
         // Ended -> Ended + (Continuing | Ending)
         | Idle, Unknown :: cs, r :: rs -> countRec Idle cs (r :: rs) + countRec (advance r) cs rs
         // Continuing -> (Continuing | Ending)
         | Continuing r, (Damaged | Unknown) :: cs, rs -> countRec (advance r) cs rs
         // Ending -> Ended
         | Ending, (Operational | Unknown) :: cs, rs -> countRec Idle cs rs
         // Good end cases
         | (Idle | Ending), [], [] -> 1L
         // Illegal state / bad end cases
         | _ -> 0L)
      
   countRec Idle initialConditions initialGroups

let expandEntry (conditions : Condition list, groups : int list) =
   conditions @ (conditions |> Seq.replicate 4 |> Seq.collect (fun x -> Unknown :: x) |> List.ofSeq),
   groups |> Seq.replicate 5 |> Seq.collect id |> List.ofSeq

let part1 () = entries |> Seq.sumBy count

let part2 () = entries |> Seq.map expandEntry |> Seq.sumBy count

let part1Expected = 8075L

let part2Expected = 4_232_520_187_524L
