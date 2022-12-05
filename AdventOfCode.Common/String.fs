namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module String =
    let isEmpty x = x = ""
    
    let isWhitespaceOrEmpty (x : string) = x.Trim() = ""
