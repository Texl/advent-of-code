/// https://adventofcode.com/2024/day/17
module AdventOfCode2024.Day17

open AdventOfCode.Common

let data = EmbeddedResource.loadText "Data/Day17.txt"

let testData =
   seq {
      "Register A: 729"
      "Register B: 0"
      "Register C: 0"
      ""
      "Program: 0,1,5,4,3,0"      
   }

let testData2 =
   seq {
      "Register A: 117440"
      "Register B: 0"
      "Register C: 0"
      ""
      "Program: 0,3,5,4,3,0"      
   }

type LiteralOperand =
   | Literal of int64

type ComboOperand =
   | ComboLiteral of int64
   | RegisterA
   | RegisterB
   | RegisterC
   | Reserved

type Instruction =
   | Adv of ComboOperand
   | Bxl of LiteralOperand
   | Bst of ComboOperand
   | Jnz of LiteralOperand
   | Bxc
   | Out of ComboOperand
   | Bdv of ComboOperand
   | Cdv of ComboOperand
 
type VM =
   { InstructionPointer : int
     A : int64
     B : int64
     C : int64
     Instructions : Instruction array
     OutputBufferR : int64 list }

let literal b =
   if b |> inRangeInclusive 0 7
   then LiteralOperand.Literal b
   else raise invalidInput
   
let combo b =
   match b with
   | 0 -> ComboLiteral 0
   | 1 -> ComboLiteral 1
   | 2 -> ComboLiteral 2
   | 3 -> ComboLiteral 3
   | 4 -> RegisterA
   | 5 -> RegisterB
   | 6 -> RegisterC
   | 7 -> Reserved
   | _ -> raise invalidInput

let decodeOp a b : Instruction =
   match a with
   | 0 -> Adv (combo b)
   | 1 -> Bxl (literal b)
   | 2 -> Bst (combo b)
   | 3 -> Jnz (literal b)
   | 4 -> Bxc
   | 5 -> Out (combo b)
   | 6 -> Bdv (combo b)
   | 7 -> Cdv (combo b)
   | _ -> raise invalidInput
   
let initialState =
   data
   |> Seq.toList
   |> List.split String.isEmpty
   |> function
      | [ registerData; programData ] ->
         let a, b, c =
            match registerData with
            | [ Regex "Register A: (-?\d+)" [ Int64 a ]
                Regex "Register B: (-?\d+)" [ Int64 b ]
                Regex "Register C: (-?\d+)" [ Int64 c ] ] ->
               a, b, c
            | _ -> raise invalidInput
         
         let instructions =
            match programData |> Seq.exactlyOne with
            | Regex "Program: (.*)" [ instructionData ] ->
               instructionData
               |> String.split ","
               |> Seq.map int
               |> Seq.chunkBySize 2
               |> Seq.map (function
                  | [| a; b |] -> decodeOp a b
                  | _ -> raise invalidInput)
               |> Seq.toArray
            | _ -> raise invalidInput
         
         { InstructionPointer = 0
           A = a
           B = b
           C = c
           Instructions = instructions
           OutputBufferR = [] }
      | _ -> raise invalidInput

let evalLiteral (operand : LiteralOperand) (vm : VM) =
   match operand with
   | LiteralOperand.Literal value -> value

let evalCombo (operand : ComboOperand) (vm : VM) =
   match operand with
   | ComboLiteral value -> value
   | RegisterA -> vm.A
   | RegisterB -> vm.B
   | RegisterC -> vm.C
   | Reserved -> raise invalidInput

let adv operand (vm : VM) =
   let op1 = vm.A
   let op2 = evalCombo operand vm
   { vm with InstructionPointer = vm.InstructionPointer + 1
             A = op1 >>> int op2 }

let bxl operand (vm : VM) =
   let op1 = vm.B
   let op2 = evalLiteral operand vm
   { vm with InstructionPointer = vm.InstructionPointer + 1
             B = op1 ^^^ op2 }

let bst operand (vm : VM) =
   let op1 = evalCombo operand vm
   { vm with InstructionPointer = vm.InstructionPointer + 1
             B = op1 % 8L }
   
let jnz operand (vm : VM) =
   if vm.A = 0 then
      { vm with InstructionPointer = vm.InstructionPointer + 1 }
   else
      { vm with InstructionPointer = int (evalLiteral operand vm / 2L) }

let bxc (vm : VM) =
   let op1 = vm.B
   let op2 = vm.C
   { vm with InstructionPointer = vm.InstructionPointer + 1
             B = op1 ^^^ op2 }

let out operand (vm : VM) =
   let op1 = evalCombo operand vm
   { vm with InstructionPointer = vm.InstructionPointer + 1
             OutputBufferR = op1 % 8L :: vm.OutputBufferR }

let bdv operand (vm : VM) =
   let op1 = vm.A
   let op2 = evalCombo operand vm
   { vm with InstructionPointer = vm.InstructionPointer + 1
             B = op1 >>> int op2 }

let cdv operand (vm : VM) =
   let op1 = vm.A
   let op2 = evalCombo operand vm
   { vm with InstructionPointer = vm.InstructionPointer + 1
             C = op1 >>> int op2 }

let simulate initialState =
   let mutable state = initialState
   while state.InstructionPointer < state.Instructions.Length do
      let instruction = state.Instructions[state.InstructionPointer]
   
      let newState =
         match instruction with
         | Adv operand -> adv operand state
         | Bxl operand -> bxl operand state
         | Bst operand -> bst operand state
         | Jnz operand -> jnz operand state
         | Bxc -> bxc state
         | Out operand -> out operand state
         | Bdv operand -> bdv operand state
         | Cdv operand -> cdv operand state
         
      state <- newState
   state.OutputBufferR |> List.rev
   
let part1 () =
   simulate initialState |> List.map string |> String.concat ","

let simulate_fast (n : int64) =
   let mutable outR = []
   let mutable a = n
   while a <> 0L do
      let a0 = a &&& 7
      let b1 = a0 ^^^ 6
      let b3 = a0 ^^^ (a >>> int b1) ^^^ 2
      outR <- (b3 &&& 7) :: outR
      a <- a >>> 3
   outR |> List.rev

let part2 () =
   let replacementValue = 0b010_100_101_011_010_101_011_100_011_101_100_000_000_011_101_110L // 90938893795566L
   simulate_fast replacementValue |> List.map string |> String.concat ","

let part1Expected = "2,3,6,2,1,6,1,2,1"

let part2Expected = "2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0"
