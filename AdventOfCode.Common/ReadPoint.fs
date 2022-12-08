namespace AdventOfCode.Common

open System
open System.Collections
open System.Collections.Generic

[<AllowNullLiteral>]
type ReadPoint<'a> (value, enumerator : IEnumerator<'a>, next : ReadPoint<'a>) =

    let isAtEnd = enumerator = null && next = null

    let mutable mNext : ReadPoint<'a> = next

    static let atEnd = ReadPoint(Unchecked.defaultof<'a>, null, null)

    static member AtEnd : ReadPoint<'a> = atEnd

    new (value : 'a) = ReadPoint(value, null, null)

    new (value, enumerator : IEnumerator<'a>) = ReadPoint(value, enumerator, null)

    new (value : 'a, next : ReadPoint<'a>) = ReadPoint(value, null, next)

    member this.IsAtEnd : bool = isAtEnd

    member this.Value : 'a =
        if isAtEnd
        then raise <| InvalidOperationException "Value undefined - enumeration at end."
        else value

    member this.TryValue =
        if isAtEnd
        then ValueNone
        else ValueSome value

    member this.Next : ReadPoint<'a> =
        // don't lock if it's already there
        if mNext <> null then
            mNext
        else
            lock this (fun () ->
                // check inside lock
                if mNext <> null then
                    mNext
                elif enumerator <> null then
                    if enumerator.MoveNext()
                    then ReadPoint(enumerator.Current, enumerator)
                    else atEnd
                    |> tap (fun next -> mNext <- next)
                else
                    raise <| InvalidOperationException "Can't advance - enumeration already at end.")

    member this.TryValueAndNext : struct ('a * ReadPoint<'a>) voption =
        if isAtEnd
        then ValueNone
        else ValueSome struct (value, this.Next)

    member this.Map (f : 'a -> 'b) =
        let mapEnumerator (f : 'a -> 'b) (enumerator : IEnumerator<'a>) =
            { new IEnumerator<_> with
                member _.Current =
                    enumerator.Current |> f

              interface IEnumerator with
                member _.Current = enumerator.Current |> f |> box

                member _.MoveNext () = enumerator.MoveNext()

                member _.Reset () = enumerator.Reset()

              interface IDisposable with
                member _.Dispose() = enumerator.Dispose() }

        let mapReadPoint (f : 'a -> 'b) (readPoint : ReadPoint<'a>) =
            let mutable current = readPoint

            { new IEnumerator<_> with
                member _.Current = current.Value |> f

              interface IEnumerator with
                member _.Current = current.Value |> f |> box

                member _.MoveNext () =
                    current <- current.Next
                    not current.IsAtEnd

                member _.Reset () = raise <| NotSupportedException()

              interface IDisposable with
                member _.Dispose() = () }

        let mappedEnumerator =
            if mNext <> null then mapReadPoint f mNext
            elif enumerator <> null then mapEnumerator f enumerator
            else mapReadPoint f ReadPoint.AtEnd

        ReadPoint(f this.Value, mappedEnumerator)



[<RequireQualifiedAccess>]
module ReadPoint =

    let atEnd<'a> =
        ReadPoint<'a>.AtEnd

    let ofEnumerator (enumerator : IEnumerator<_>) =
        if enumerator.MoveNext()
        then ReadPoint(enumerator.Current, enumerator)
        else atEnd

    let ofSeq (source : seq<'a>) =
        ofEnumerator (source.GetEnumerator())

    type private EnumeratorState =
        | NotStarted
        | AtValue
        | AtEnd
        | PastEnd

    let toSeq (initialReadPoint : ReadPoint<_>) : seq<'a>  =
        let inline getEnumerator () =
            let mutable state = struct (NotStarted, initialReadPoint)

            let getValue () =
                match state with
                | NotStarted, _ -> failwith "Value undefined - enumeration not started"
                | AtValue, readPoint -> readPoint.Value
                | AtEnd, _ -> failwith "Value undefined - enumeration at end"
                | PastEnd, _ -> failwith "Value undefined - enumeration past end"

            { new IEnumerator<_> with
                member _.Current = getValue ()

              interface IEnumerator with
                member _.Current = getValue () |> box

                member _.MoveNext () =
                    let advanceTo (next : ReadPoint<_>) =
                        let nextState =
                            if next = null || next.IsAtEnd
                            then PastEnd
                            else AtValue

                        state <- struct (nextState, next)
                        next <> null && not next.IsAtEnd

                    match state with
                    | NotStarted, initial -> advanceTo initial
                    | AtValue, current -> advanceTo current.Next
                    | AtEnd, _ -> advanceTo null
                    | PastEnd, _ -> raise <| InvalidOperationException "Can't advance - enumeration already past end."

                member _.Reset () = raise <| NotSupportedException()

              interface IDisposable with
                member _.Dispose() = () }

        { new IEnumerable<_> with
                member _.GetEnumerator() = getEnumerator ()

          interface IEnumerable with
            member _.GetEnumerator() = getEnumerator () :> IEnumerator }

    let toReadPointSeq (initialReadPoint : ReadPoint<_>) : seq<ReadPoint<_>>  =
        let inline getEnumerator () =
            let mutable state = struct (NotStarted, initialReadPoint)

            let getValue () =
                match state with
                | NotStarted, _ -> failwith "Value undefined - enumeration not started"
                | AtValue, readPoint -> readPoint
                | AtEnd, readPoint -> readPoint
                | PastEnd, _ -> failwith "Value undefined - enumeration past end"

            { new IEnumerator<_> with
                member _.Current = getValue ()

              interface IEnumerator with
                member _.Current = getValue () |> box

                member _.MoveNext () =
                    let advanceTo (next : ReadPoint<_>) =
                        let newState =
                            if next = null
                            then PastEnd
                            elif next.IsAtEnd
                            then AtEnd
                            else AtValue

                        state <- struct (newState, next)
                        next <> null

                    match state with
                    | NotStarted, initial -> advanceTo initial
                    | AtValue, current -> advanceTo current.Next
                    | AtEnd, _ -> advanceTo null
                    | PastEnd, _ -> raise <| InvalidOperationException "Can't advance - enumeration already past end."

                member _.Reset () = raise <| NotSupportedException()

              interface IDisposable with
                member _.Dispose() = () }

        { new IEnumerable<_> with
            member _.GetEnumerator() = getEnumerator ()

          interface IEnumerable with
            member _.GetEnumerator() = getEnumerator () :> IEnumerator }

    let head (v: ReadPoint<_>) = v.Value

    let tail (v: ReadPoint<_>) = v.Next

    let (|Cons|AtEnd|) (readPoint: ReadPoint<'a>) =
        match readPoint.TryValueAndNext with
        | ValueSome (value, next) -> Cons (value, next)
        | ValueNone -> AtEnd

    let rpcons (head : 'a) (tail : ReadPoint<'a>) =
        ReadPoint(head, tail)

    let map f (v: ReadPoint<_>) = v.Map(f)

    let rec transpose (source : ReadPoint<ReadPoint<'a>>) =
        match source with
        | Cons (Cons (_, _), _) as m -> rpcons (map head m) (transpose (map tail m))
        | _ -> atEnd
