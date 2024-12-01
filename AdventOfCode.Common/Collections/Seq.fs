namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module RuntimeHelpers =
   open System.Collections.Generic
  
   [<Struct; NoComparison; NoEquality>]
   type internal StructBox<'T when 'T:equality>(value:'T) =
      member x.Value = value
      static member Comparer =
         let gcomparer = HashIdentity.Structural<'T>
         { new IEqualityComparer<StructBox<'T>> with
            member __.GetHashCode(v) = gcomparer.GetHashCode(v.Value)
            member __.Equals(v1,v2) = gcomparer.Equals(v1.Value,v2.Value) }


[<RequireQualifiedAccess>]
module Seq =
   open System.Collections
   open System.Collections.Generic
  
   let collecti f = Seq.indexed >> Seq.collect (fun (ord, x) -> f ord x)
   
   let choosei f = Seq.indexed >> Seq.choose (fun (ord, x) -> f ord x)

   let tails (source : seq<_>) =
      source
      |> ReadPoint.ofSeq
      |> ReadPoint.toReadPointSeq
      |> Seq.map ReadPoint.toSeq

   let cache2 (s : seq<_>) = s |> ReadPoint.ofSeq

   let cons head tail =
      seq {
         yield head
         yield! tail
      }

   let (|Cons|Empty|) (source : seq<_>) =
      match source with
      | :? ReadPoint<_> as readPoint -> readPoint
      | source -> source |> ReadPoint.ofSeq
      |> function
         | ReadPoint.Cons(value, next) -> Cons(value, next)
         | ReadPoint.AtEnd -> Empty

   let unzip (source : seq<'a * 'b>) : seq<'a> * seq<'b> =
      let cached = source |> cache2
      cached |> ReadPoint.toSeq |> Seq.map fst,
      cached |> ReadPoint.toSeq |> Seq.map snd

   let unzip3 (source : seq<'a * 'b * 'c>) : seq<'a> * seq<'b> * seq<'c> =
      let cached = source |> cache2

      cached |> ReadPoint.toSeq |> Seq.map (fun (a, _, _) -> a),
      cached |> ReadPoint.toSeq |> Seq.map (fun (_, b, _) -> b),
      cached |> ReadPoint.toSeq |> Seq.map (fun (_, _, c) -> c)

   // groupBy clone / modify

   [<AutoOpen>]
   module Internals =
      let mkSeq f =
         { new IEnumerable<'U> with
            member x.GetEnumerator() = f()
           interface IEnumerable with
            member x.GetEnumerator() = (f() :> IEnumerator) }
         
      let mkDelayedSeq (f: unit -> IEnumerable<'T>) =
         mkSeq (fun () -> f().GetEnumerator())
   
   let inline chunkByImpl
      (comparer : IEqualityComparer<'SafeKey>)
      ([<InlineIfLambda>] keyf : 'T -> 'SafeKey)
      ([<InlineIfLambda>] getKey : 'SafeKey -> 'Key)
      (source : seq<'T>) =
      checkNonNull "seq" source

      let mutable state = ValueNone

      seq {
         for v in source do
            let safeKey = keyf v

            match state with
            | ValueNone ->
               state <- ValueSome (safeKey, [ v ])
            | ValueSome (lastSafeKey, currR) ->
               if comparer.Equals(lastSafeKey, safeKey) then
                  state <- ValueSome (safeKey, v :: currR)
               else
                  yield (getKey lastSafeKey, List.rev currR)
                  state <- ValueSome (safeKey, [ v ])
            
         match state with
         | ValueSome (lastSafeKey, currR) ->
            yield (getKey lastSafeKey, List.rev currR)
         | ValueNone -> ()
      }

   let chunkByValueType (keyf: 'T -> 'Key) (seq: seq<'T>) =
      seq
      |> chunkByImpl HashIdentity.Structural<'Key> keyf id
                
   // Wrap a StructBox around all keys in case the key type is itself a type using null as a representation
   let chunkByRefType (keyf: 'T -> 'Key) (seq: seq<'T>) =
      seq
      |> chunkByImpl
         RuntimeHelpers.StructBox<'Key>.Comparer
         (fun t -> RuntimeHelpers.StructBox(keyf t))
         (fun sb -> sb.Value)
   
   let chunkBy (projection: 'T -> 'Key) (source: seq<'T>) =
      if typeof<'Key>.IsValueType then
         mkDelayedSeq (fun () -> chunkByValueType projection source)
      else
         mkDelayedSeq (fun () -> chunkByRefType projection source)
