namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module Func =
    open System.Collections.Concurrent
    open System.Runtime.CompilerServices

    let memoize (f : 'a -> 'b) : 'a -> 'b =
        let cache = ConcurrentDictionary()
        fun x -> cache.GetOrAdd(x, lazy f x).Value

    let memoizeRec (f : ('a -> 'b) -> 'a -> 'b) : 'a -> 'b =
        let cache = ConcurrentDictionary()
        let rec recF x =
            cache.GetOrAdd(x, lazy f recF x).Value
        recF

    let memoizeWeak (f : 'a -> 'b) : 'a -> 'b =
        let cache = ConditionalWeakTable()
        fun x -> cache.GetValue(x, fun x -> lazy f x).Value

    let memoizeWeakRec (f : ('a -> 'b) -> 'a -> 'b) : 'a -> 'b =
        let cache = ConditionalWeakTable()
        let rec recF x =
            cache.GetValue(x, fun x -> lazy f recF x).Value
        recF

    let repeat (n : int) (f : 'a -> 'a) : 'a -> 'a =
        Seq.replicate n f
        |> Seq.reduce (>>)
