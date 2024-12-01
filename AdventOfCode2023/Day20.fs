/// https://adventofcode.com/2023/day/20
module AdventOfCode2023.Day20

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day20.txt"

type ModuleTypeInitial =
   | FlipFlopInitial
   | ConjunctionInitial
   | BroadcastInitial
   | ButtonInitial
   | UntypedInitial
   | RxInitial

type ModuleInitial =
   { Type : ModuleTypeInitial
     Name : string
     Outputs : string list }

let modulesInitial =
   let parsed =
      data
      |> Seq.map (function
         | Regex "(.*) -> (.*)" [ typeNameStr; outputsStr ] ->
            let moduleType, name =
               match typeNameStr with
               | StartsWith "%" name -> FlipFlopInitial, name
               | StartsWith "&" name -> ConjunctionInitial, name
               | name when name = "broadcaster" -> BroadcastInitial, name
               | name -> UntypedInitial, name
               
            let outputs = outputsStr |> String.split "," |> Seq.map String.trim |> List.ofSeq
            
            { Type = moduleType
              Name = name
              Outputs = outputs }
         | x -> raiseInvalidInput x)
      |> List.ofSeq
      
   let button =
      { Type = ButtonInitial
        Name = "Button"
        Outputs = [ "broadcaster" ] }
      
   let output =
      { Type = UntypedInitial
        Name = "output"
        Outputs = [] }
      
   let rx =
      { Type = RxInitial
        Name = "rx"
        Outputs = [] }
      
   rx :: output :: button :: parsed

type Pulse =
   | Low
   | High

type FlipFlopState =
   | On
   | Off

type ModuleType =
   | FlipFlop of FlipFlopState
   | Conjunction of Map<string, Pulse>
   | Broadcast
   | Button
   | Untyped
   | Rx

type Module =
   { Type : ModuleType
     Name : string
     Inputs  : string list
     Outputs : string list }

type PulseTick =
   { Source : string
     Target : string
     Pulse : Pulse }

let modules =
   let targetToInputs =
      modulesInitial
      |> Seq.collect (fun source ->
         source.Outputs
         |> Seq.map (fun target ->
            source.Name, target))
      |> Seq.groupBy snd
      |> Seq.map (mapSnd (Seq.map fst >> List.ofSeq))
      |> Map.ofSeq
   
   modulesInitial
   |> List.map (fun moduleInitial ->
      let inputs =
         targetToInputs
         |> Map.tryFind moduleInitial.Name
         |> Option.defaultValue []
      
      let moduleType =
         match moduleInitial.Type with
         | FlipFlopInitial -> FlipFlop Off
         | ConjunctionInitial -> Conjunction (inputs |> Seq.map (fun input -> input, Low) |> Map.ofSeq)
         | BroadcastInitial -> Broadcast
         | ButtonInitial -> Button
         | UntypedInitial -> Untyped
         | RxInitial -> Rx
      
      { Type = moduleType
        Name = moduleInitial.Name
        Inputs = inputs
        Outputs = moduleInitial.Outputs })
   |> List.map (fun m -> m.Name, m)
   |> Map.ofSeq
   
let tickm (pulseTick : PulseTick) (ms : Map<string, Module>) : Map<string, Module> * PulseTick list =
   let target = ms |> Map.find pulseTick.Target
   
   let nextTargetTypeState, broadcastPulse =
      match target.Type, pulseTick.Pulse with
      | FlipFlop state, High -> FlipFlop state, None
      | FlipFlop Off, Low -> FlipFlop On, Some Pulse.High
      | FlipFlop On, Low -> FlipFlop Off, Some Pulse.Low
      | Conjunction inputStates, pulse ->
         let source = ms |> Map.find pulseTick.Source
         let newState = inputStates |> Map.add source.Name pulse

         let generatedPulse =
            if newState |> Map.forall (fun _ v -> v = Pulse.High)
            then
               // printfn $"& -{pulse}-> {target.Name}"
               Some Pulse.Low
            else Some Pulse.High
         Conjunction newState, generatedPulse
      | Broadcast, pulse -> Broadcast, Some pulse
      | Button, _ -> failwith "Button shouldn't receive pulses"
      | Untyped, _ -> Untyped, None
      | Rx, pulse ->
         // printfn $"Rx: {pulse}"
         Rx, None

   let newModules =
      ms
      |> Map.add target.Name { target with Type = nextTargetTypeState }
   
   let generatedPulses =
      match broadcastPulse with
      | Some newPulse ->
         target.Outputs
         |> Seq.map (fun newTarget ->
            { Source = target.Name
              Target = newTarget
              Pulse = newPulse })
         |> List.ofSeq
      | None -> []
      
   newModules, generatedPulses

let rec tick (ms : Map<string, Module>) accl acch (pulseTicks : PulseTick list) =
   match pulseTicks with
   | pulseTick :: remainingPulseTicks ->
      let newModuleStates, addedPulseTicks = tickm pulseTick ms
      
      let naccl, nacch =
         match pulseTick.Pulse with
         | Low -> accl + 1, acch 
         | High -> accl, acch + 1 
      
      tick newModuleStates naccl nacch (remainingPulseTicks @ addedPulseTicks)
   | _ ->
      ms, accl, acch

let initialPulseTick =
   { Source = "button"
     Target = "broadcaster"
     Pulse = Low }
   
let part1 () =
   let countPulses () =
      let mutable lows = 0
      let mutable highs = 0
      let mutable state = modules
      for i in 1 .. 1_000 do
         let newState, newlows, newhighs = tick state 0 0 [ initialPulseTick ]
         state <- newState
         lows <-  lows + newlows
         highs <- highs + newhighs
   
      lows * highs
   
   countPulses ()
   
let part2 () =
   let cycle state =
      let newState, _, _ = tick state 0 0 [ initialPulseTick ]
      newState

   let rec runToCycle (accumR : _ list) fstate state =
       let nextState = cycle state
       let nextStateComp = fstate nextState
       match accumR |> List.tryFindIndex ((=) nextStateComp) with
       | Some idx -> accumR.Length - idx, accumR |> List.rev
       | None -> runToCycle (nextStateComp :: accumR) fstate nextState

   let rxInputName = modules |> Map.find "rx" |> fun m -> m.Inputs |> List.exactlyOne
   
   let rxInput = modules |> Map.find rxInputName
   
   let cycleNames = rxInput.Inputs

   let rec gather seen queue =
      seq {
         match queue with
         | curName :: rem ->
            let cur = modules |> Map.find curName
            if seen |> Set.contains cur.Name |> not then
               yield cur.Name
            yield! gather (seen |> Set.add cur.Name) (rem @ (cur.Inputs |> List.filter (fun n -> seen |> Set.contains n |> not)))
         | [] -> ()
      }
      
   let cycleDetects =
      cycleNames
      |> Seq.map (fun cycleName ->
         let ffNames = gather Set.empty [ cycleName ] |> List.ofSeq
         
         let _, cycleSeq =
            let fstate m = ffNames |> List.map (fun n -> m |> Map.find n)            
            runToCycle [] fstate modules

         int64 cycleSeq.Length)
      |> Seq.reduce lcm
   
   cycleDetects

let part1Expected = 680_278_040

let part2Expected = 243_548_140_870_057L
