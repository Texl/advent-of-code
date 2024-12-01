namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module Dict =
    open System.Collections.Generic
    open System.Linq

    let ofSeq (source: seq<'k * 'v>) : IDictionary<'k, 'v> =
        source.ToDictionary(fst, snd) :> IDictionary<_, _>

    let containsKey (key: 'k) (dict: IDictionary<'k, 'v>) = dict.ContainsKey(key)

    let find (key: 'k) (dict: IDictionary<'k, 'v>) : 'v =
        match dict.TryGetValue(key) with
        | true, value -> value
        | false, _ ->
            let sampleKeys = dict.Keys |> Seq.truncate 10 |> Array.ofSeq
            failwith $"Couldn't find {key} in dictionary with keys %A{sampleKeys}"

    let tryFind (key: 'k) (dict: IDictionary<'k, 'v>) : 'v option =
        match dict.TryGetValue(key) with
        | true, value -> Some value
        | false, _ -> None
