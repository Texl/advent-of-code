namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module TryParse =
    open System
    open System.Text.RegularExpressions

    let private tryParseWith f s =
        match f (string s) with
        | true, v -> Some v
        | _ -> None

    let uint8 s = tryParseWith FSharp.Core.uint8.TryParse s
    let uint16 s = tryParseWith FSharp.Core.uint16.TryParse s
    let uint s = tryParseWith FSharp.Core.uint.TryParse s
    let uint64 s = tryParseWith FSharp.Core.uint64.TryParse s
    let int8 s = tryParseWith FSharp.Core.int8.TryParse s
    let int16 s = tryParseWith FSharp.Core.int16.TryParse s
    let int s = tryParseWith FSharp.Core.int.TryParse s
    let int64 s = tryParseWith FSharp.Core.int64.TryParse s
    let bigint s = tryParseWith FSharp.Core.bigint.TryParse s
    let float32 s = tryParseWith FSharp.Core.float32.TryParse s
    let float s = tryParseWith FSharp.Core.float.TryParse s
    let decimal s = tryParseWith FSharp.Core.decimal.TryParse s

    let enum<'a when 'a : (new: unit -> 'a) and 'a : struct and 'a :> ValueType> (s : string) =
        tryParseWith Enum.TryParse<'a> s
        
    let enumI<'a when 'a : (new: unit -> 'a) and 'a : struct and 'a :> ValueType> (s : string) =
        tryParseWith (fun (s : string) -> Enum.TryParse<'a>(s, ignoreCase=true)) s

    let regex pattern s =
        let m = Regex.Match(s, pattern)
        if m.Success
        then Some (m.Groups |> Seq.map (fun g -> g.Value) |> Seq.tail |> List.ofSeq)
        else None

    let regexes pattern s =
        let matches = Regex.Matches(s, pattern)
        if matches.Count > 0
        then Some (matches |> Seq.collect (fun m -> m.Groups |> Seq.map (fun g -> g.Value) |> Seq.tail) |> List.ofSeq)
        else None


[<AutoOpen>]
module TryParseAPs =
    let (|Uint8|_|) s : uint8 option = TryParse.uint8 s
    let (|Uint16|_|) s : uint16 option = TryParse.uint16 s
    let (|Uint|_|) s : uint option = TryParse.uint s
    let (|Uint64|_|) s : uint64 option = TryParse.uint64 s
    let (|Int8|_|) s : int8 option = TryParse.int8 s
    let (|Int16|_|) s : int16 option = TryParse.int16 s
    let (|Int|_|) s : int option = TryParse.int s
    let (|Int64|_|) s : int64 option = TryParse.int64 s
    let (|BigInt|_|) s : bigint option = TryParse.bigint s
    let (|Float32|_|) s : float32 option = TryParse.float32 s
    let (|Float|_|) s : float option = TryParse.float s
    let (|Decimal|_|) s : decimal option = TryParse.decimal s

    let (|Uint32|_|) = (|Uint|_|)
    let (|Int32|_|) = (|Int|_|)
    let (|Real32|_|) = (|Float32|_|)
    let (|Real64|_|) = (|Float|_|)
    let (|Single|_|) = (|Float32|_|)
    let (|Double|_|) = (|Float|_|)
    
    let (|Enum|_|) s : 'a option = TryParse.enum s
    let (|EnumI|_|) s : 'a option = TryParse.enumI s

    let (|Regex|_|) (pattern : string) s : string list option = TryParse.regex pattern s

    let (|Regexes|_|) (pattern : string) s : string list option = TryParse.regexes pattern s

    let (|StartsWith|_|) (prefix : string) (s : string) : string option =
        if s.StartsWith prefix
        then Some <| s.Substring(prefix.Length)
        else None


[<RequireQualifiedAccess>]
module Parse =
    open System.Runtime.CompilerServices
    open System.Runtime.InteropServices

    type private Solidify () =
        static member X (o : 'a option, [<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string) =
            match o with
            | Some v -> v
            | None -> failwith $"{memberName} failed: {o}"

    let uint8 s = s |> TryParse.uint8 |> Solidify.X
    let uint16 s = s |> TryParse.uint16 |> Solidify.X
    let uint s = s |> TryParse.uint |> Solidify.X
    let uint64 s = s |> TryParse.uint64 |> Solidify.X
    let int8 s = s |> TryParse.int8 |> Solidify.X
    let int16 s = s |> TryParse.int16 |> Solidify.X
    let int s = s |> TryParse.int |> Solidify.X
    let int64 s = s |> TryParse.int64 |> Solidify.X
    let bigint s = s |> TryParse.bigint |> Solidify.X
    let float32 s = s |> TryParse.float32 |> Solidify.X
    let float s = s |> TryParse.float |> Solidify.X
    let decimal s = s |> TryParse.decimal |> Solidify.X
    let regex pattern s = s |> TryParse.regex pattern |> Solidify.X
