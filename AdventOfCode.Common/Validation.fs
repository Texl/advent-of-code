namespace AdventOfCode.Common

[<AutoOpen>]
module Validation =
   let inline checkNonNull argName arg =
      match box arg with
      | null -> nullArg argName
      | _ -> ()

   let invalidInput = Failure "Invalid input"

   let raiseInvalidInput invalidInput = raise (Failure $"Invalid input: {invalidInput}")

   let unreachable = Failure "Unreachable"
