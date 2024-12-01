/// https://adventofcode.com/2023/day/19
module AdventOfCode2023.Day19

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day19.txt"

type Operand1 = X | M | A | S

type Operand2 = Literal of int

type Operator = LT | GT
   
type Result = Accepted | Rejected | GoTo of string

type WorkflowStage =
   { Operand1 : Operand1
     Operand2 : Operand2
     Operator : Operator
     Result : Result }

type Workflow =
   { Name : string
     Stages : WorkflowStage list
     Else : Result }

type Part =
   { X : int
     M : int
     A : int
     S : int }
   with member this.Rating = this.X + this.M + this.A + this.S

let (|Operand1|_|) str =
   match str with
   | "x" -> Some X
   | "m" -> Some M
   | "a" -> Some A
   | "s" -> Some S
   | _ -> None

let (|Operand2|_|) str =
   match str with
   | Int x -> Some (Literal x)
   | _ -> None

let (|Operator|_|) str =
   match str with
   | "<" -> Some LT
   | ">" -> Some GT
   | _ -> None
   
let (|Result|) str =
   match str with
   | "A" -> Accepted
   | "R" -> Rejected
   | x -> (GoTo x)
   
let parse data =
   let parseWorkflowStages str =
      str
      |> String.split ","
      |> Seq.choose (function
         | Regex "(.*)(<|>)(.*):(.*)" [ Operand1 o1; Operator opr; Operand2 o2; Result res ] ->
            Some { Operand1 = o1; Operator = opr; Operand2 = o2; Result = res }
         | _ -> None)
      |> List.ofSeq

   let parseWorkflows str =
      str
      |> List.choose (function
         | Regex "(\w+)\{(.*),(.*)\}" [ name; workflowsStr; Result elseResult ] ->
            { Name = name
              Stages = parseWorkflowStages workflowsStr
              Else = elseResult }
            |> Some
         | _ -> None)

   let parseParts str =
      str
      |> List.choose (function
         | Regex "\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}" [ Int x; Int m; Int a; Int s ] ->
            { X = x
              M = m
              A = a
              S = s }
            |> Some
         | _ -> None)
      
   data
   |> List.ofSeq
   |> List.split String.isEmpty
   |> function
      | [ workflowsStr; partsStr ] ->
         let workflows = parseWorkflows workflowsStr
         let parts = parseParts partsStr
         let workflowLookup = workflows |> Seq.map (fun wf -> wf.Name, wf) |> Map.ofSeq
         workflowLookup, parts
      | _ -> raise invalidInput

let part1 () =
   let evaluateWorkflowStage (part : Part) (workflowStage : WorkflowStage) =
      let operand1 =
         match workflowStage.Operand1 with
         | X -> part.X
         | M -> part.M
         | A -> part.A
         | S -> part.S
      
      let operand2 =
         match workflowStage.Operand2 with
         | Literal x -> x

      let func =
         match workflowStage.Operator with
         | LT -> (<)
         | GT -> (>)
      
      if func operand1 operand2
      then Some workflowStage.Result
      else None

   let evaluateWorkflow (part : Part) (workflow : Workflow) =
      let rec evalWfStep remStages =
         match remStages with
         | [] -> workflow.Else, part
         | st :: newRemStages ->
            match evaluateWorkflowStage part st with
            | Some stResult -> stResult, part
            | None -> evalWfStep newRemStages
            
      evalWfStep workflow.Stages

   let evaluateWorkflows (workflows : Map<string, Workflow>) initialPart =
      let rec processor accepted remaining =
         match remaining with
         | (curRes, curPart) :: newRemaining ->
            match curRes with
            | GoTo next ->
               let addResults = workflows |> Map.find next |> evaluateWorkflow curPart |> List.singleton
               processor accepted (newRemaining @ addResults)
               
            | Accepted ->
               processor (curPart :: accepted) newRemaining
               
            | Rejected ->
               processor accepted newRemaining
         | _ ->
            accepted |> List.sumBy (fun hp -> hp.Rating)
         
      processor [] [ GoTo "in", initialPart ]

   let workflows, parts = parse data

   parts
   |> Seq.sumBy (evaluateWorkflows workflows)

type HypotheticalPart =
   { X : int[]
     M : int[]
     A : int[]
     S : int[] }
   with
   member this.Combinations =
      [ this.X; this.M; this.A; this.S ]
      |> Seq.map Array.length
      |> Seq.map int64
      |> Seq.reduce (*)

let part2 () =
   let evaluateWorkflowStage (part : HypotheticalPart) (workflowStage : WorkflowStage) =
      let operand1 =
         match workflowStage.Operand1 with
         | X -> part.X
         | M -> part.M
         | A -> part.A
         | S -> part.S
      
      let operand2 =
         match workflowStage.Operand2 with
         | Literal x -> x

      let func =
         match workflowStage.Operator with
         | LT -> (<)
         | GT -> (>)

      let trues, falses = operand1 |> Array.partition (fun v -> func v operand2)
      
      match workflowStage.Operand1 with
      | X -> { part with X = trues }, { part with X = falses }
      | M -> { part with M = trues }, { part with M = falses }
      | A -> { part with A = trues }, { part with A = falses }
      | S -> { part with S = trues }, { part with S = falses }

   let evaluateWorkflow (part : HypotheticalPart) (workflow : Workflow) =
      let mutable results : (Result * HypotheticalPart) list = []
      
      let rec evalWfStep subpart remStages =
         match remStages with
         | [] ->
            results <- (workflow.Else, subpart) :: results
         | st :: newRemStages ->
            let subpartTrue, subpartFalse = evaluateWorkflowStage subpart st
            results <- (st.Result, subpartTrue) :: results
            evalWfStep subpartFalse newRemStages
            
      evalWfStep part workflow.Stages
      results

   let evaluateWorkflows (workflows : Map<string, Workflow>) =
      let rec processor accepted remaining =
         match remaining with
         | (curRes, curPart) :: newRemaining ->
            match curRes with
            | GoTo next ->
               let addResults = workflows |> Map.find next |> evaluateWorkflow curPart
               processor accepted (newRemaining @ addResults)
               
            | Accepted ->
               processor (curPart :: accepted) newRemaining
               
            | Rejected ->
               processor accepted newRemaining
         | _ ->
            accepted |> List.sumBy (fun part -> part.Combinations)
         
      let initialPart =
         { X = Array.init 4_000 (fun x -> x + 1)
           M = Array.init 4_000 (fun x -> x + 1)
           A = Array.init 4_000 (fun x -> x + 1)
           S = Array.init 4_000 (fun x -> x + 1) }
         
      processor [] [ GoTo "in", initialPart ]

   data |> parse |> fst |> evaluateWorkflows
   
let part1Expected = 487_623

let part2Expected = 113_550_238_315_130L
