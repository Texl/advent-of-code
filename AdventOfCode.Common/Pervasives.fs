namespace AdventOfCode.Common

[<AutoOpen>]
module Pervasives =

    let flip f a b = f b a

    let mapFst f (a, b) = f a, b

    let mapSnd f (a, b) = a, f b
