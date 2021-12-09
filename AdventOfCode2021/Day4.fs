namespace AdventOfCode2021

// https://adventofcode.com/2021/day/4

module Day4 =

    [<AutoOpen>]
    module Day4Data =

        open System
        open System.Text.RegularExpressions

        let dataFilePath = "Data/Day4.data"

        type BingoBoard =
            { Numbers : int list list }

        let (randomNumbers : int list), (bingoBoards : BingoBoard list) =
            let lines =
                dataFilePath
                |> Utils.loadTextResource
                |> List.ofSeq
                
            let randomSequence =
                lines.Head.Split ','
                |> Seq.map int
                |> List.ofSeq

            let bingoBoards =
                lines
                |> String.concat "\n"
                |> Regex("(\n(\s*\d+){5}){5}").Matches
                |> Seq.map (fun m ->
                    let group = m.Groups.Item 1

                    let lines = group.Captures |> Seq.map (fun capture -> capture.Value)

                    let boardNumbers =
                        lines
                        |> Seq.map (fun line ->
                            line.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                            |> Seq.map int
                            |> List.ofSeq)
                        |> List.ofSeq
                        
                    { Numbers = boardNumbers } : BingoBoard)
                |> List.ofSeq
                
            randomSequence, bingoBoards
    
//    --- Day 4: Giant Squid ---
//
//    You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.
//
//    Maybe it wants to play bingo?
//
//    Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)
//
//    The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:
//
//    7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
//
//    22 13 17 11  0
//     8  2 23  4 24
//    21  9 14 16  7
//     6 10  3 18  5
//     1 12 20 15 19
//
//     3 15  0  2 22
//     9 18 13 17  5
//    19  8  7 25 23
//    20 11 10 24  4
//    14 21 16 12  6
//
//    14 21 17 24  4
//    10 16 15  9 19
//    18  8 23 26 20
//    22 11 13  6  5
//     2  0 12  3  7
//
//    After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):
//
//    22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
//     8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
//    21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
//     6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
//     1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
//
//    After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:
//
//    22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
//     8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
//    21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
//     6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
//     1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
//
//    Finally, 24 is drawn:
//
//    22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
//     8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
//    21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
//     6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
//     1 12 20 15 19        14 21 16 12  6         2  0 12  3  7
//
//    At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).
//
//    The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.
//
//    To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

    type Coordinate =
        | Horizontal of int
        | Vertical of int
    
//    type Coordinates = { V : int; H : int }
    
    type WinCombination =
        { Pattern : Set<int>
          State : Set<int> }
    
    type BingoBoardState =
        { Identity : int
          BingoBoard : BingoBoard
          Lookup : Map<int, Coordinate[]>
          WinCombinations : Map<Coordinate, WinCombination> }
    
    let part1 () =
        
        let initialBoardStates =
            bingoBoards
            |> List.mapi (fun boardIdentity bingoBoard ->
                let lookup =
                    bingoBoard.Numbers
                    |> Seq.collecti (fun h -> Seq.mapi (fun v n -> n, [| Horizontal h; Vertical v |]))
                    |> Map.ofSeq
               
                let winCombinations =
                    seq {
                        yield!
                            bingoBoard.Numbers
                            |> Seq.mapi (fun h row ->
                                Horizontal h,
                                { Pattern = row |> Set.ofList
                                  State = Set.empty })

                        yield!
                            bingoBoard.Numbers
                            |> Seq.transpose
                            |> Seq.mapi (fun v row ->
                                Vertical v,
                                { Pattern = row |> Set.ofSeq
                                  State = Set.empty })
                    }
                    |> Map.ofSeq
                    
                { Identity = boardIdentity
                  BingoBoard = bingoBoard
                  Lookup = lookup
                  WinCombinations = winCombinations })
        
        let processCalledNumber (calledNumber : int) (boardState : BingoBoardState) =
            match boardState.Lookup |> Map.tryFind calledNumber with
            | Some coordinates ->
                (boardState, coordinates)
                ||> Array.fold (fun boardStateSoFar coordinate ->
                    let newWinCombinations =
                        let newWinCombination =
                            let winCombination = boardStateSoFar.WinCombinations.[coordinate]
                            let newState = winCombination.State.Add calledNumber
                            { winCombination with State = newState }
                        boardStateSoFar.WinCombinations.Add (coordinate, newWinCombination) 
                    { boardStateSoFar with WinCombinations = newWinCombinations })
            | None -> boardState
        
        let isWinningBoardState (boardState : BingoBoardState) =
            boardState.WinCombinations
            |> Map.toSeq
            |> Seq.exists (fun (_coordinate, winCombination) -> winCombination.Pattern = winCombination.State)

        let getWinningBoardScore (numbersCalledR : int list) (boardState : BingoBoardState) =
            let unmarkedNumbers =
                let allBingoBoardNumbers = boardState.BingoBoard.Numbers |> Seq.collect id |> Set.ofSeq
                let allNumbersCalled = numbersCalledR |> Set.ofList
                allBingoBoardNumbers - allNumbersCalled
            // Score is the sum of unmarkes numbers multiplied by the last called number                         
            (unmarkedNumbers |> Seq.sum) * numbersCalledR.Head
        
        let winningScore, winningBoardState =
            (initialBoardStates, (randomNumbers, []))
            |> Seq.unfold (fun (boardStates, (numbersRemaining, numbersCalledR)) ->
                match numbersRemaining with
                | number :: newNumbersRemaining ->

                    let newNumbersCalledR = number :: numbersCalledR
                    
                    // all boards process called number n
                    let newBoardStates =
                        boardStates
                        |> List.map (processCalledNumber number)

                    // detect winning board. If one exists, return an empty numbersLeftToProcess to end the unfold on the next iteration.  
                    let maybeWinningScoreAndBoardState, numbersLeftToProcess =
                        match newBoardStates |> List.filter isWinningBoardState with
                        | [] -> None, newNumbersRemaining
                        | [ winningBoardState ] -> Some (getWinningBoardScore newNumbersCalledR winningBoardState, winningBoardState), []
                        | _ -> failwith "Multiple winners - unexpected condition?"
                    
                    Some (maybeWinningScoreAndBoardState, (newBoardStates, (numbersLeftToProcess, newNumbersCalledR)))
                | [] -> None)
            // Cull the leading Nones
            |> Seq.choose id
            |> Seq.tryExactlyOne
            |> Option.defaultWith (fun () -> failwith "No winner - unexpected condition?")

        printfn $"Winning board ID: {winningBoardState.Identity}"
        printfn $"Winning board score: {winningScore}"
        printfn $"Winning board state: %A{winningBoardState}"

        ()
