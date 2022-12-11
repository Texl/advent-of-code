namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module String =
    [<Literal>]
    let empty = ""

    let isEmpty x = x = empty
    
    let isWhitespaceOrEmpty (x : string) = x.Trim() = empty

    let replace (target: string) (replacement: string) (str: string) = str.Replace(target, replacement)

    let split (str: string) (separator: string) = str.Split(separator)
