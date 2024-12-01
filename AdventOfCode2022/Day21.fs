namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/21
module Day21 =
    let data = EmbeddedResource.loadText "Data/Day21.txt"

    type Op = Root | Add | Sub | Mul | Div
    type Number = Monkey of int64 | Human of int64
    type Monkey = Op of Op * Monkey * Monkey | Num of Number

    let root : Monkey =
        let configMonkeyById =
            let (|Op|_|) =
                function
                | "+" -> Some Add
                | "-" -> Some Sub
                | "*" -> Some Mul
                | "/" -> Some Div
                | _ -> None
            
            data
            |> Seq.map (function
                | Regex "(root)\: (\w+) \+ (\w+)" [ id; a; b ] -> id, Choice1Of2 (Root, a, b)
                | Regex "(\w+)\: (\w+) (.) (\w+)" [ id; a; Op op; b ] -> id, Choice1Of2 (op, a, b)
                | Regex "(humn)\: (\w+)" [ id; Int64 a ] -> id, Choice2Of2 (Human a)
                | Regex "(\w+)\: (\-*\d+)" [ id; Int64 a ] -> id, Choice2Of2 (Monkey a)
                | _ -> raise invalidInput)
            |> Map.ofSeq
        
        let rec hydrate id : Monkey =
            match configMonkeyById[id] with
            | Choice1Of2 (f, a, b) -> Op (f, hydrate a, hydrate b)
            | Choice2Of2 n -> Num n

        hydrate "root"
    
    let part1 () =
        let rec eval =
            function
            | Op (Root, a, b)
            | Op (Add, a, b) -> eval a + eval b
            | Op (Sub, a, b) -> eval a - eval b
            | Op (Mul, a, b) -> eval a * eval b
            | Op (Div, a, b) -> eval a / eval b
            | Num (Human n) -> n
            | Num (Monkey n) -> n

        printfn $"{eval root}"

    let part2 () =
        let evalGuess guess =
            let rec eval =
                function
                | Op (Root, a, b) -> eval a - eval b // comparison
                | Op (Add, a, b) -> eval a + eval b
                | Op (Sub, a, b) -> eval a - eval b
                | Op (Mul, a, b) -> eval a * eval b
                | Op (Div, a, b) -> eval a / eval b
                | Num (Human _) -> guess // ignoring input data
                | Num (Monkey n) -> n
            eval root
            
        let min = 0L
        let max = 1_000_000_000_000_000L

        let rec search lower upper =
            if upper - lower = 1L then
                search max min // wrong direction; reverse it
            else
                let half = (upper - lower) / 2L + lower
                match evalGuess half with
                | 0L -> half
                | r when r > 0L -> search half upper
                | _ -> search lower half

        printfn $"{search min max}"
