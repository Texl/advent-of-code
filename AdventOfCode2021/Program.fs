namespace AdventOfCode2021

module Program =

    let header (day : int) (part : int) =
        printfn ""
        printfn $"Day {day}, Part {part}"
        
        
    [<EntryPoint>]
    let main _argv =

        header 1 1
        Day1.part1 ()
        header 1 2
        Day1.part2 ()

        header 2 1
        Day2.part1 ()
        header 2 2
        Day2.part2 ()

        header 3 1
        Day3.part1 ()
        header 3 2
        Day3.part2 ()
        
        header 4 1
        Day4.part1 ()

        header 4 2
        Day4.part2 ()
        
        header 5 1
        Day5.part1 ()

        0
