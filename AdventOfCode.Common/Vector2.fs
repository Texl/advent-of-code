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
        static member One =
            { R = 1
              C = 1 }
        static member UnitR =
            { R = 1
              C = 0 }
        static member UnitC =
            { R = 0
              C = 1 }
        static member (+) (a : Vector2, b : Vector2) =
            { R = a.R + b.R
              C = a.C + b.C }
        static member (-) (a : Vector2, b : Vector2) =
            { R = a.R - b.R
              C = a.C - b.C }
        static member inline (*) (a : Vector2, b : 'a) =
            { R = a.R * int64 b
              C = a.C * int64 b }
        static member inline (*) (a : 'a, b : Vector2) =
            { R = int64 a * b.R
              C = int64 a * b.C }
