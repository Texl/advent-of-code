namespace AdventOfCode.Common

[<AutoOpen>]
module ReflectionUtil =
   open System
   open System.Reflection
   open System.Runtime.CompilerServices
   open Microsoft.FSharp.Reflection

   let private cache = ConditionalWeakTable<Type, obj>()
   let private memoize (key : Type) (f : unit -> 'v) =
      cache.GetValue(key, fun key -> f () :> obj) :?> 'v

   let getAllUnionCases<'a> () =
      memoize typeof<'a> (fun () ->
         try
            FSharpType.GetUnionCases(typeof<'a>, BindingFlags.Public)
            |> Array.map (fun unionCaseInfo ->
               FSharpValue.MakeUnion(unionCaseInfo, [||]) :?> 'a)
         with exn -> [||])
