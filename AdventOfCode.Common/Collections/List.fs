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

    let diagonals (m : _ list list) =
      [
         let rMax = m.Length - 1
         let cMax = m[0].Length - 1
         
         for rInit in 0 .. rMax do
            [ for r = rInit downto 0 do
               yield m[r][rInit - r] ]
            
         for cInit in 1 .. cMax do
            [ for c = cInit to cMax do
               yield m[cMax + cInit - c][c] ]
      ]