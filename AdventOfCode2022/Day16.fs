namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/16
module Day16 =
    let data = EmbeddedResource.loadText "Data/Day16.txt"

    type Valve =
        { Identity : string
          Flow : int64
          Tunnels : string list }

    let valves =
        data
        |> Seq.map (function
            | Regex "Valve (\w\w) has flow rate=(\d+); tunnel[s]* lead[s]* to valve[s]* (.*)" [ identity; Int64 flow; tunnelsStr ] ->
                let tunnels = tunnelsStr.Split(',') |> Seq.map String.trim |> List.ofSeq
                { Identity = identity
                  Flow = flow
                  Tunnels = tunnels }
            | junk -> failwith $"invalid: {junk}")
        |> List.ofSeq
    
    let findValve identity = valves |> List.find (fun v -> v.Identity = identity)

    let startValve = findValve "AA"
    
    let paths =
        let significantValves = valves |> List.filter (fun v -> v.Flow > 0) |> Set.ofSeq
    
        let rec getDistance (level : Set<Valve>) (target : Valve) (depth : int64) =
            if level.Contains target then
                depth
            else
                let nextLevel = level |> Seq.collect (fun v -> v.Tunnels |> Seq.map findValve) |> Set.ofSeq
                getDistance nextLevel target (depth + 1L)

        significantValves
        |> Set.add startValve
        |> Seq.map (fun source ->
            source,
            significantValves
            |> Seq.filter (fun target -> target <> source)
            |> Seq.map (fun target -> target, getDistance (Set.singleton source) target 0L)
            |> Map.ofSeq)
        |> Map.ofSeq

    let part1 () =
        let initialTimeRemaining = 29
        
        let mutable best = 0L
        let rec search (currentValve : Valve) (openedValves : Set<Valve>) (totalFlow : int64) (remainingMinutes : int64) =
            best <- max totalFlow best
            
            if remainingMinutes > 0 then
                if openedValves |> Set.contains currentValve then
                    paths
                    |> Map.find currentValve
                    |> Map.filter (fun v _ -> not (openedValves.Contains v))
                    |> Seq.iter (fun (KeyValue (v, distance)) -> search v openedValves totalFlow (remainingMinutes - distance))
                else
                    let newFlow = totalFlow + currentValve.Flow * remainingMinutes
                    search currentValve (openedValves |> Set.add currentValve) newFlow (remainingMinutes - 1L)

        search startValve (Set.singleton startValve) 0 initialTimeRemaining

        best |> printfn "%d"

    type Phase =
        | Me
        | Elephant
    
    let part2 () =
        let initialTimeRemaining = 25

        let mutable best = 0L
        let rec search (currentValve : Valve) (openedValves : Set<Valve>) (totalFlow : int64) (remainingMinutes : int64) (phase : Phase) =
            best <- max totalFlow best
            
            if remainingMinutes > 0 then
                if openedValves |> Set.contains currentValve then
                    paths
                    |> Map.find currentValve
                    |> Map.filter (fun v _ -> not (openedValves.Contains v))
                    |> Seq.iter (fun (KeyValue (v, distance)) -> search v openedValves totalFlow (remainingMinutes - distance) phase)
                else
                    let newFlow = totalFlow + currentValve.Flow * remainingMinutes
                    search currentValve (openedValves |> Set.add currentValve) newFlow (remainingMinutes - 1L) phase
                    if phase = Me then search startValve (openedValves |> Set.add currentValve) newFlow initialTimeRemaining Elephant

        search startValve (Set.singleton startValve) 0 initialTimeRemaining Me
            
        best |> printfn "%d"
