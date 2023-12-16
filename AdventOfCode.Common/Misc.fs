namespace AdventOfCode.Common

[<AutoOpen>]
module Misc =
    let invalidInput = Failure "Invalid input"
    let raiseInvalidInput invalidInput = raise (Failure $"Invalid input: {invalidInput}")
