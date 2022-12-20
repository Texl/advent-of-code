namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/20
module Day20 =
    let data = EmbeddedResource.loadText "Data/Day20.txt"

    let input = data |> Seq.map int64 |> Array.ofSeq
    
    let inline modulo a b = (a % b + b) % b

    let fPermute (origin : int) (destination : int) (cur : int) =
        if cur = origin then
            destination
        elif origin < destination && cur > origin && cur <= destination then
            cur - 1
        elif origin > destination && cur < origin && cur >= destination then
            cur + 1
        else cur

    let mix (key : int64) (iterations : int) (input : int64[]) =
        let mutable working = 
            input
            |> Array.mapi (fun ord x ->
                {| OriginalIndex = ord
                   Keyed = x * key
                   Offset = x * key |})

        let len = working.Length
                
        for iteration in 1..iterations do
            for index in 0..len - 1 do
                let origin = working |> Array.findIndex (fun elt -> elt.OriginalIndex = index)
                let destination = int (modulo (int64 origin + working[origin].Offset) (int64 (len - 1)))
                working <- working |> Array.permute (fPermute origin destination)

        working |> Array.map (fun elt -> elt.Keyed)
        
    let part1 () =
        let mixed = input |> mix 1L 1

        let coordinates =
            let zeroIndex = mixed |> Array.findIndex ((=) 0L)
            [| 1000; 2000; 3000 |]
            |> Array.sumBy (fun zeroOffset -> mixed[modulo (zeroIndex + zeroOffset) mixed.Length])

        printfn $"%d{coordinates}"

    let part2 () =
        let mixed = input |> mix 811_589_153L 10

        let coordinates =
            let zeroIndex = mixed |> Array.findIndex ((=) 0L)
            [| 1000; 2000; 3000 |]
            |> Array.sumBy (fun zeroOffset -> mixed[modulo (zeroIndex + zeroOffset) mixed.Length])

        printfn $"%d{coordinates}"
