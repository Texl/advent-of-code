namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module Regex =
   open System.Linq
   open System.Text.RegularExpressions
   
   let tryMatch (pattern: string) (str: string) =
      let m = Regex.Match(str, pattern)
      if m.Success then
         m.Groups
         |> Seq.map (fun g ->  g.Name, g.Captures.OfType<Capture>() |> Array.ofSeq)
         |> dict
         |> Some
      else
         None
