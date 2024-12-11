namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module String =
   open System

   [<Literal>]
   let empty = ""

   let isEmpty x = x = empty

   let isWhitespaceOrEmpty (x : string) = x.Trim() = empty

   let replace (target : string) (replacement : string) (str : string) = str.Replace(target, replacement)

   let split (separator : string) (str : string) = str.Split(separator, StringSplitOptions.RemoveEmptyEntries)

   let splitAt (index : int) (str : string) = str.Substring(0, index), str.Substring(index, str.Length - index) 

   let trim (str : string) = str.Trim()

   let tails (str : string) =
      seq {
         for i in 0 .. str.Length - 1 do
            yield str.Substring(i)
      }

   let endsWith (suffix : string) (str : string) = str.EndsWith(suffix)
