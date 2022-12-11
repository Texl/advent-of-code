namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/11
module Day11 =

    let data = EmbeddedResource.loadText "Data/Day11.txt"

    type Monkey =
        { Index : int
          StartingItems : int64 list
          Operation : int64 -> int64
          DivisibleByTest : int64
          IfTrue : int
          IfFalse : int  }

    let parseMonkey (lines : string[]) =
        let index =
            match lines[0] with
            | Regex "Monkey (\d*):" [ Int x ] -> x
            | _ -> raise invalidInput
        
        let startingItems =
            match lines[1] with
            | Regex "\s*Starting items: (.*)" [ x ] -> x.Split ',' |> Array.map int64 |> List.ofArray
            | _ -> raise invalidInput

        let operation =
            match lines[2] with
            | Regex "\s*Operation: new = old (\*|\+) (old|\d*)" [ operator; operand ] ->
                let f =
                    match operator with
                    | "*" -> (*)
                    | "+" -> (+)
                    | _ -> raise invalidInput

                match operand with
                | Int64 n -> fun x -> f x n
                | "old" -> fun x -> f x x
                | _ -> raise invalidInput
            | _ -> raise invalidInput
            
        let divisibleByTest =
            match lines[3] with
            | Regex "\s*Test: divisible by (\d*)" [ Int64 x ] -> x
            | _ -> raise invalidInput
            
        let ifTrue =
            match lines[4] with
            | Regex "\s*If true: throw to monkey (\d*)" [ Int x ] -> x
            | _ -> raise invalidInput

        let ifFalse =
            match lines[5] with
            | Regex "\s*If false: throw to monkey (\d*)" [ Int x ] -> x
            | _ -> raise invalidInput

        { Index = index
          StartingItems = startingItems
          Operation = operation
          DivisibleByTest = divisibleByTest
          IfTrue = ifTrue
          IfFalse = ifFalse }

    let monkeys =
        data
        |> Array.ofSeq
        |> Array.split (fun s -> s = "")
        |> Array.map parseMonkey
        
    type MonkeyState =
        { Items : int64 list
          Inspections : int64
          Monkey : Monkey }
        static member Create monkey =
            { Items = monkey.StartingItems
              Inspections = 0L
              Monkey = monkey }
            
    type Throw =
        { TargetIndex : int
          Item : int64 }
        
    let getRounds (fReduce : int64 -> int64) =
        let takeMonkeyTurn (state : MonkeyState) =
            let throws =
                state.Items
                |> List.map (fun item -> 
                    let newItem = item |> state.Monkey.Operation |> fReduce
                    let targetIndex =
                        if newItem % state.Monkey.DivisibleByTest = 0
                        then state.Monkey.IfTrue
                        else state.Monkey.IfFalse
                    { TargetIndex = targetIndex
                      Item = newItem })

            let newState =
                { state with
                    Items = []
                    Inspections = state.Inspections + int64 state.Items.Length }
            
            throws, newState

        let applyThrows (throws : Throw list) (states : MonkeyState[]) =
            let targetIndexToThrows =
                throws
                |> Seq.groupBy (fun throw -> throw.TargetIndex)
                |> Seq.map (mapSnd List.ofSeq)
                |> Map.ofSeq

            states
            |> Array.mapi (fun index state ->
                match targetIndexToThrows |> Map.tryFind index with
                | Some throws -> { state with Items = state.Items @ (throws |> List.map (fun throw -> throw.Item)) }
                | None -> state)

        monkeys
        |> Array.map MonkeyState.Create
        |> Seq.unfold (fun states ->
            let newStates =
                (states, [ 0 .. states.Length - 1 ])
                ||> Seq.fold (fun currentStates i ->
                    let throws, newState = currentStates[i] |> takeMonkeyTurn

                    currentStates
                    // update current monkey state
                    |> Array.mapi (fun j state -> if i = j then newState else state)
                    // apply throws to all monkey states
                    |> applyThrows throws)
            Some (newStates, newStates))
            
    let getMonkeyBusiness rounds fReduce =
        let finalRound = getRounds fReduce |> Seq.truncate rounds |> Seq.last
        finalRound |> Seq.map (fun m -> m.Inspections) |> Seq.sortDescending |> Seq.truncate 2 |> Seq.reduce (*)
            
    let part1 () =
        let rounds = 20
        let fReduce x = x / 3L
        getMonkeyBusiness rounds fReduce
        |> printfn "%d"

    let part2 () =
        let rounds = 10_000
        // lcm of primes is just their product; for non-primes this isn't the LCM but would still work
        let lcm = monkeys |> Seq.map (fun m -> m.DivisibleByTest) |> Seq.reduce (*)
        let fReduce x = x % lcm
        getMonkeyBusiness rounds fReduce
        |> printfn "%d"
