namespace AdventOfCode.Common

[<AutoOpen>]
module Pervasives =

   let tap f v =
      f v
      v

   let flip f a b = f b a

   let mapFst f (a, b) = f a, b

   let mapSnd f (a, b) = a, f b

   let inline konst _ v = v

   let inline w f x = f x x


[<AutoOpen>]
module Math =
   let inline modulo a b = (a % b + b) % b

   // greatest common divisor
   let rec gcd a b = if b = 0L then a else gcd b (a % b)

   // least common multiple
   let inline lcm a b = a * b / gcd a b


[<AutoOpen>]
module Validation =
   let inline checkNonNull argName arg =
      match box arg with
      | null -> nullArg argName
      | _ -> ()
