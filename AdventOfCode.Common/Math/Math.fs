namespace AdventOfCode.Common

[<AutoOpen>]
module Math =
   let inline modulo a b = (a % b + b) % b

   // greatest common divisor
   let inline gcd a b =
      let zero = LanguagePrimitives.GenericZero
        
      let rec _gcd a b =
         if b = zero 
         then a
         else _gcd b (a % b)

      _gcd a b
        
   // least common multiple
   let inline lcm a b = a * b / gcd a b
   
   let inline inRangeInclusive lo hi a = lo <= a && a <= hi
   
   let inline inRangeExclusive lo hi a = lo < a && a < hi
