namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module Parse =
    open System.Text.RegularExpressions

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
