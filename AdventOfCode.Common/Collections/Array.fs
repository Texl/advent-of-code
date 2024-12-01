namespace AdventOfCode.Common

[<RequireQualifiedAccess>]
module Array =
    let split pred (xs : #seq<'a>) =
        let f elt (acc, buf) =
            match pred elt, buf with
            | false, buf -> acc, elt :: buf
            | true, [] -> acc, []
            | true, buf -> buf :: acc, []
            
        match (xs, ([], [])) ||> Seq.foldBack f with
        | acc, [] -> acc
        | acc, buf -> buf :: acc
        |> Seq.map Array.ofList
        |> Array.ofSeq

    let toTuple2 xs =
        match xs |> Array.truncate 2 with
        | [| a; b |] -> a, b
        | _ -> failwith "Expected 2 elements" 
            
    let toTuple3 xs =
        match xs |> Array.truncate 3 with
        | [| a; b; c |] -> a, b, c
        | _ -> failwith "Expected 3 elements" 
