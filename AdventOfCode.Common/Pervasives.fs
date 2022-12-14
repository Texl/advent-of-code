namespace AdventOfCode.Common

[<AutoOpen>]
module Pervasives =

    let tap f v = f v; v

    let flip f a b = f b a

    let mapFst f (a, b) = f a, b

    let mapSnd f (a, b) = a, f b
    
    let inline konst _ v = v
