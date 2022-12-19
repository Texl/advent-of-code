namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module Seq =
    let collecti f = Seq.indexed >> Seq.collect (fun (ord, x) -> f ord x)

    let tails (source: seq<_>) =
        source
        |> ReadPoint.ofSeq
        |> ReadPoint.toReadPointSeq
        |> Seq.map ReadPoint.toSeq

    let cache2 (s: seq<_>) = s |> ReadPoint.ofSeq

    let cons head tail =
        seq {
            yield head
            yield! tail
        }

    let (|Cons|Empty|) (source: seq<_>) =
        match source with
        | :? ReadPoint<_> as readPoint -> readPoint
        | source -> source |> ReadPoint.ofSeq
        |> function
            | ReadPoint.Cons (value, next) -> Cons (value, next)
            | ReadPoint.AtEnd -> Empty

    let unzip (source: seq<'a * 'b>) : seq<'a> * seq<'b> =
        let cached = source |> cache2
        cached |> ReadPoint.toSeq |> Seq.map fst,
        cached |> ReadPoint.toSeq |> Seq.map snd

    let unzip3 (source: seq<'a * 'b * 'c>) : seq<'a> * seq<'b> * seq<'c> =
        let cached = source |> cache2
        cached |> ReadPoint.toSeq |> Seq.map (fun (a, _, _) -> a),
        cached |> ReadPoint.toSeq |> Seq.map (fun (_, b, _) -> b),
        cached |> ReadPoint.toSeq |> Seq.map (fun (_, _, c) -> c)
