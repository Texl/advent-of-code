namespace Ascalon

[<RequireQualifiedAccess>]
module Func =
    open System.Collections.Concurrent
    open System.Runtime.CompilerServices

    let memoize f =
        let cache = ConcurrentDictionary()
        fun x -> cache.GetOrAdd(x, lazy f x).Value

    let memoizeRec f =
        let cache = ConcurrentDictionary()
        let rec recF x =
            cache.GetOrAdd(x, lazy f recF x).Value
        recF

    let memoizeWeak f =
        let cache = ConditionalWeakTable()
        fun x -> cache.GetValue(x, fun x -> lazy f x).Value

    let memoizeWeakRec f =
        let cache = ConditionalWeakTable()
        let rec recF x =
            cache.GetValue(x, fun x -> lazy f x).Value
        recF
