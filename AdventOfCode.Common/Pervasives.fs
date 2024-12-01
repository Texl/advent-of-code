namespace AdventOfCode.Common

[<AutoOpen>]
module Pervasives =

   let tap f v =
      f v
      v

   let flip f a b = f b a

   let mapFst f (a, b) = f a, b

   let mapSnd f (a, b) = a, f b

   let mapBoth f (a, b) = f a, f b
   
   let applyBoth f g a = f a, g a

   let inline konst _ v = v

   let inline w f x = f x x
