namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module Seq =

    let collecti f = Seq.indexed >> Seq.collect (fun (ord, x) -> f ord x)
