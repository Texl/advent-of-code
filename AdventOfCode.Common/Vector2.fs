namespace AdventOfCode.Common

[<AutoOpen>]
module Vector2Types =
    type Vector2 =
        { R : int64
          C : int64 }
        member this.ToTuple () =
            this.R, this.C
        static member Zero =
            { R = 0
              C = 0 }
        static member (+) (a : Vector2, b : Vector2) =
            { R = a.R + b.R
              C = a.C + b.C }
        static member (-) (a : Vector2, b : Vector2) =
            { R = a.R - b.R
              C = a.C - b.C }
        static member (*) (a : Vector2, b : int64) =
            { R = a.R * b
              C = a.C * b }
