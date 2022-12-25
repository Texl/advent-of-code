namespace AdventOfCode.Common

/// Option computation expression
[<AutoOpen>]
module OptionCE =

    type OptionCEBuilder() =
        member _.Bind(v, f) = Option.bind f v
        member _.Combine (a, b) = Option.orElse b a
        member _.Delay f = f ()
        member _.Return x = Some x
        member _.ReturnFrom x = x
        member _.Zero() = None

    let opt = OptionCEBuilder()


[<AutoOpen>]
module ValueOptionCE =

    type ValueOptionCEBuilder() =
        member _.Bind(v, f) = ValueOption.bind f v
        member _.Return x = ValueSome x
        member _.ReturnFrom x = x
        member _.Zero() = ValueNone

    let vopt = ValueOptionCEBuilder()


[<RequireQualifiedAccess>]
module Option =
    open System

    let inline sum (o : ^A option) =
        o |> Option.defaultValue LanguagePrimitives.GenericZero

    let inline sumBy (f : ^A -> ^B) (o : ^A option) =
        o |> Option.map f |> sum

    let ofSeq (s: #seq<_>) =
        use e = s.GetEnumerator()

        if e.MoveNext() then
            let firstValue = e.Current

            if e.MoveNext() then
                raise <| ArgumentException $"Expected a sequence with length 0 or 1 - observed length was {s |> Seq.length}"
            else
                Some firstValue
        else
            None
