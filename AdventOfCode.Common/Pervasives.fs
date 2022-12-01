namespace AdventOfCode.Common

[<AutoOpen>]
module Pervasives =

    let mapFst f (a, b) = f a, b

    let mapSnd f (a, b) = a, f b
