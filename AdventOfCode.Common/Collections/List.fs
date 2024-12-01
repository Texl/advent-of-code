namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module List =
    let split pred (xs : #seq<'a>) =
        let f elt (acc, buf) =
            match pred elt, buf with
            | false, buf -> acc, elt :: buf
            | true, [] -> acc, []
            | true, buf -> buf :: acc, []
            
        match (xs, ([], [])) ||> Seq.foldBack f with
        | acc, [] -> acc
        | acc, buf -> buf :: acc
