namespace AdventOfCode2022

open AdventOfCode.Common

/// https://adventofcode.com/2022/day/19
module Day19 =
    let data = EmbeddedResource.loadText "Data/Day19.txt"

    let testData =
        [ "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
          "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian." ]

    type Resource =
        | Ore
        | Clay
        | Obsidian
        | Geode

    let (|Resource|_|) str =
        match str with
        | "ore" -> Some Ore
        | "clay" -> Some Clay
        | "obsidian" -> Some Obsidian
        | "geode" -> Some Geode
        | _ -> None
    
    type ResourceCost =
        { Resource : Resource
          Cost : int }

    type ResourceIndex =
        | Ore = 0
        | Clay = 1
        | Obsidian = 2
        | Geode = 3
    
    type Blueprint =
        { Id : int
          Costs : int[][]
          MaxOreCost : int }

    let input =
        data
        |> Seq.map (fun line ->
            let blueprintId =
                match line with
                | Regex "Blueprint (\d*):" [ Int blueprintId ] -> blueprintId
                | _ -> raise invalidInput
            
            let resourceToRobotCosts =
                line
                |> String.split "."
                |> Seq.choose (function
                    | Regex "(ore|clay|obsidian|geode) robot costs (.*)" [ Resource resource; costStr ] ->
                        let resourceCosts =
                            costStr
                            |> String.split " and "
                            |> Seq.choose (function
                                | Regex "(\d+) (ore|clay|obsidian|geode)" [ Int count; Resource resource ] ->
                                    { Resource = resource
                                      Cost = count }
                                    |> Some
                                | _ -> None)
                            |> List.ofSeq
                            
                        Some (resource, resourceCosts)
                    | _ -> None)
                |> Map.ofSeq

            let costs =
                [| ResourceIndex.Ore, Ore
                   ResourceIndex.Clay, Clay
                   ResourceIndex.Obsidian, Obsidian
                   ResourceIndex.Geode, Geode |]
                |> Array.map (fun (_, res) ->
                    let cost = resourceToRobotCosts[res]
                    [| match (cost |> List.tryFind (fun x -> x.Resource = Ore)) with Some v -> v.Cost | None -> 0
                       match (cost |> List.tryFind (fun x -> x.Resource = Clay)) with Some v -> v.Cost | None -> 0
                       match (cost |> List.tryFind (fun x -> x.Resource = Obsidian)) with Some v -> v.Cost | None -> 0
                       match (cost |> List.tryFind (fun x -> x.Resource = Geode)) with Some v -> v.Cost | None -> 0 |])
            
            let costT = costs |> Array.transpose
            
            let maxOreCost =
                costT[0]
                |> Seq.mapi (fun ord v -> if ord = 0 then 0 else v) // exclude self because we apparently never care about optimizing ore to the fullest
                |> Seq.max
            
            { Id = blueprintId
              Costs = costs
              MaxOreCost = maxOreCost })
        |> List.ofSeq
    
    type State =
        { Blueprint : Blueprint
          Resources : int[]
          Robots : int[]
          mutable Minute : int }
        static member Initial minutes blueprint =
            { Blueprint = blueprint
              Resources = [| 0; 0; 0; 0 |]
              Robots = [| 1; 0; 0; 0 |]
              Minute = minutes }

        member this.AddRobot resource =
            let index =
                match resource with
                | Ore -> ResourceIndex.Ore
                | Clay -> ResourceIndex.Clay
                | Obsidian -> ResourceIndex.Obsidian
                | Geode -> ResourceIndex.Geode
            this.Robots[int index] <- this.Robots[int index] + 1
            for i in 0..3 do
                this.Resources[i] <- this.Resources[i] - this.Blueprint.Costs[int index][i]

        member this.Tick () =
            for i in 0..3 do
                this.Resources[i] <- this.Resources[i] + this.Robots[i]
            this.Minute <- this.Minute - 1
            
        member this.Clone () =
            { Blueprint = this.Blueprint
              Resources = this.Resources |> Array.copy
              Robots = this.Robots |> Array.copy
              Minute = this.Minute }
    
    let resourceIndexes =
        [| ResourceIndex.Ore
           ResourceIndex.Clay
           ResourceIndex.Obsidian
           ResourceIndex.Geode |]

    [<System.Flags>]
    type ResourceFlags =
        | None = 0
        | Ore = 1
        | Clay = 2
        | Obsidian = 4
        | Geode = 8
        | All = 15
        
    let part1 () =
    
        let triangularNumbers = Array.init 100 (fun i -> (i + 1) * (i + 2) / 2)
        
        let triangularPartialSums = (0, triangularNumbers) ||> Seq.scan (+) |> Array.ofSeq
        
        let doit useQuality =
            input
            |> (fun bps ->
                if useQuality then
                    bps
                else
                    bps |> List.truncate 3)
            |> List.map (fun blueprint ->
                let mutable bestResult = 0
                let mutable evaluated = 0

                let mutable treeEstimate = System.Collections.Generic.Dictionary<int, int>()
                let addTree n =
                    if treeEstimate.ContainsKey n then
                        treeEstimate[n] <- treeEstimate[n] + 1
                    else
                        treeEstimate[n] <- 1
                
                let getHasAChance state =
                    let currentGeode = state.Resources[int ResourceIndex.Geode]
                    let remainingMinutes = state.Minute
                    let maxPotential = triangularPartialSums[remainingMinutes]
                    let incoming =  + state.Robots[int ResourceIndex.Geode] * remainingMinutes
                    currentGeode + incoming + maxPotential > bestResult

                let getDoWant state allowed =
                    let canAfford i =
                        state.Blueprint.Costs[i][0] <= state.Resources[0] &&
                        state.Blueprint.Costs[i][1] <= state.Resources[1] &&
                        state.Blueprint.Costs[i][2] <= state.Resources[2] &&
                        state.Blueprint.Costs[i][3] <= state.Resources[3]
                    
                    let canUseMore i =
                        match i with
                        | 0 -> state.Robots[0] < state.Blueprint.MaxOreCost
                        | 1 -> state.Robots[1] < state.Blueprint.Costs[2][1]
                        | 2 -> state.Robots[2] < state.Blueprint.Costs[3][2]
                        | _ -> true
                    
                    let isAllowed i =
                        match i with
                        | 0 -> allowed &&& ResourceFlags.Ore <> ResourceFlags.None
                        | 1 -> allowed &&& ResourceFlags.Clay <> ResourceFlags.None
                        | 2 -> allowed &&& ResourceFlags.Obsidian <> ResourceFlags.None
                        | 3 -> allowed &&& ResourceFlags.Geode <> ResourceFlags.None
                        | _ -> failwith "invalid"
                    
                    fun flag ->
                        let i = int flag
                        let affordable = canAfford i
                        let usable = canUseMore i
                        let allowable = isAllowed i
                        affordable && usable && allowable
                                
                let rec simulate minutes (oldAllowed : ResourceFlags) (state : State) =
                    addTree state.Minute

                    if state.Minute = 0 then
                        evaluated <- evaluated + 1

                        let geodes = state.Resources[int ResourceIndex.Geode]
                        if geodes > bestResult then
                            bestResult <- geodes 
                    else
                        let hasAChance = getHasAChance state

                        if hasAChance && state.Minute > 1 then
                            let mutable allowed = oldAllowed

                            let doWant = getDoWant state allowed
                            
                            if doWant ResourceIndex.Geode then
                                let stateHere = state.Clone()
                                allowed <- allowed &&& ~~~ResourceFlags.Geode
                                stateHere.Tick()
                                stateHere.AddRobot Geode
                                stateHere |> simulate minutes ResourceFlags.All
                            else
                                if doWant ResourceIndex.Obsidian then
                                    allowed <- allowed &&& ~~~ResourceFlags.Obsidian
                                    let stateHere = state.Clone()
                                    stateHere.Tick()
                                    stateHere.AddRobot Obsidian
                                    stateHere |> simulate minutes ResourceFlags.All
                                
                                if doWant ResourceIndex.Clay then
                                    allowed <- allowed &&& ~~~ResourceFlags.Clay
                                    let stateHere = state.Clone()
                                    stateHere.Tick()
                                    stateHere.AddRobot Clay
                                    stateHere |> simulate minutes ResourceFlags.All

                                if doWant ResourceIndex.Ore then
                                    allowed <- allowed &&& ~~~ResourceFlags.Ore
                                    let stateHere = state.Clone()
                                    stateHere.Tick()
                                    stateHere.AddRobot Ore
                                    stateHere |> simulate minutes ResourceFlags.All

                                if allowed <> ResourceFlags.All then
                                    let stateHere = state.Clone()
                                    stateHere.Tick()
                                    stateHere |> simulate minutes allowed
                                else
                                    let stateHere = state.Clone()
                                    stateHere.Tick()
                                    stateHere |> simulate minutes ResourceFlags.All
                        else
                            let stateHere = state.Clone()
                            stateHere.Tick()
                            stateHere |> simulate minutes ResourceFlags.All

                let duration = if useQuality then 24 else 32 
                
                let startState = State.Initial duration blueprint
                
                evaluated <- 0
                treeEstimate <- System.Collections.Generic.Dictionary<int, int>()
                startState |> simulate duration ResourceFlags.All

                printfn $"{evaluated} evaluated"           
                // treeEstimate.Keys
                // |> Seq.sort
                // |> Seq.iter (fun k -> printfn $"%2d{k} %d{treeEstimate.[k]}")
                 
                if useQuality then
                    let quality = blueprint.Id * bestResult
                    quality
                else
                    bestResult)
            |> fun x ->
                if useQuality then
                    x |> Seq.sum
                else
                    x |> Seq.reduce (*)
            |> printfn "%d"

        doit true
        doit false
