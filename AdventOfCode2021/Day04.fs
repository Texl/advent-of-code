namespace AdventOfCode2021

open AdventOfCode.Common

// https://adventofcode.com/2021/day/4

module Day04 =

    [<AutoOpen>]
    module Day4Data =

        open System
        open System.Text.RegularExpressions

        let dataFilePath = "Data/Day04.txt"

        type BingoBoard =
            { Numbers : int list list }

        let (initialNumbersRemaining : int list), (bingoBoards : BingoBoard list) =
            let lines =
                dataFilePath
                |> EmbeddedResource.loadText
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
        
    let part1 () =

        let firstWinningScore, firstWinningBoardState =
            (initialBoardStates, (initialNumbersRemaining, []))
            |> Seq.unfold (fun (boardStatesStillPlaying, (numbersRemaining, numbersCalledR)) ->
                match boardStatesStillPlaying, numbersRemaining with
                | _, [] -> failwith "Ran out of numbers to call, but there are boards still playing"
                | [], _ -> None // end of sequence - no boards are still playing
                | _, number :: newNumbersRemaining ->

                    let newNumbersCalledR = number :: numbersCalledR
                    
                    // all boards process called number
                    let newBoardStates =
                        boardStatesStillPlaying
                        |> List.map (processCalledNumber number)
                    
                    match newBoardStates |> List.partition isWinningBoardState with
                    | [], nonWinningBoardStates ->
                        // No winner yet
                        let boardStatesStillPlaying = nonWinningBoardStates
                        Some (None, (boardStatesStillPlaying, (newNumbersRemaining, newNumbersCalledR)))
 
                    | [ firstWinningBoardState ], _nonWinningBoardStates ->
                        // Found the first winner
                        let scoreAndWinningBoardState =
                            let score = getWinningBoardScore newNumbersCalledR firstWinningBoardState
                            (score, firstWinningBoardState)
                        let boardStatesStillPlaying = [] // all play stops with the first win
                        Some (Some scoreAndWinningBoardState, (boardStatesStillPlaying, (newNumbersRemaining, newNumbersCalledR)))
 
                    | winningBoardStates, _ ->
                        failwith $"{winningBoardStates.Length} first winners - unexpected condition?")
            |> Seq.choose id
            |> Seq.tryExactlyOne
            |> Option.defaultWith (fun () -> failwith "No winner - unexpected condition?")

        printfn $"First winning board is ID {firstWinningBoardState.Identity}, with a score of {firstWinningScore}"

//    On the other hand, it might be wise to try a different strategy: let the giant squid win.
//
//    You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.
//
//    In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
//
//    Figure out which board will win last. Once it wins, what would its final score be?

    let part2 () =

        let lastWinningScore, lastWinningBoardState =
            (initialBoardStates, (initialNumbersRemaining, [], []))
            |> Seq.unfold (fun (boardStatesStillPlaying, (numbersRemaining, numbersCalledR, winningBoardStatesR)) ->
                match boardStatesStillPlaying, numbersRemaining with
                | _, [] -> failwith "Ran out of numbers to call, but there are boards still playing"
                | [], _ -> None // end of sequence - no boards are still playing
                | _, number :: newNumbersRemaining ->

                    let newNumbersCalledR = number :: numbersCalledR
                    
                    // all boards process called number
                    let newBoardStates =
                        boardStatesStillPlaying
                        |> List.map (processCalledNumber number)

                    // pull out boards that just won
                    match newBoardStates |> List.partition isWinningBoardState with
                    | [ lastWinningBoardState ], [] ->
                        // Found one last winner
                        let score =  getWinningBoardScore newNumbersCalledR lastWinningBoardState
                        let win = Some (score, lastWinningBoardState)
                        let boardStatesStillPlaying = [] // all play stops with the first win
                        Some (win, (boardStatesStillPlaying, (newNumbersRemaining, newNumbersCalledR, lastWinningBoardState :: winningBoardStatesR)))

                    | _, [] ->
                        failwith "Found multiple last winners - unexpected condition?"

                    | winningBoardStates, nonWinningBoardStates ->
                        // 0-N winners with some number of nonwinners remaining
                        let boardStatesStillPlaying = nonWinningBoardStates
                        Some (None, (boardStatesStillPlaying, (newNumbersRemaining, newNumbersCalledR, winningBoardStates @ winningBoardStatesR))))
            // Cull the leading Nones
            |> Seq.choose id
            |> Seq.tryExactlyOne
            |> Option.defaultWith (fun () -> failwith "No winner - unexpected condition?")

        printfn $"First winning board is ID {lastWinningBoardState.Identity}, with a score of {lastWinningScore}"
