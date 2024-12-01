namespace AdventOfCode.Common

[<AutoOpen>]
module Vector2Types =
   open System

   [<Struct>]
   [<StructuredFormatDisplay("({R}, {C})")>]
   type Vector2i =
      { R : int
        C : int }
      static member Zero = { R = 0; C = 0 }
      static member One = { R = 1; C = 1 }
      static member UnitR = { R = 1; C = 0 }
      static member UnitC = { R = 0; C = 1 }
      static member inline (~-)(a : Vector2i) = { R = -a.R; C = -a.C }
      static member inline (+)(a : Vector2i, b : Vector2i) = { R = a.R + b.R; C = a.C + b.C }
      static member inline (-)(a : Vector2i, b : Vector2i) = { R = a.R - b.R; C = a.C - b.C }
      static member inline (*)(a : Vector2i, b) = { R = a.R * int b; C = a.C * (int b) }
      static member inline (*)(a, b : Vector2i) = { R = int a * b.R; C = int a * b.C }
      
   [<Struct>]
   [<StructuredFormatDisplay("({R}, {C})")>]
   type Vector2 =
      { R : int64
        C : int64 }
      member this.Magnitude = Math.Sqrt(float (this.R * this.R + this.C * this.C))
      static member Zero = { R = 0; C = 0 }
      static member One = { R = 1; C = 1 }
      static member UnitR = { R = 1; C = 0 }
      static member UnitC = { R = 0; C = 1 }
      static member inline (~-)(v : Vector2) = { R = -v.R; C = -v.C }
      static member inline (+)(a : Vector2, b : Vector2) = { R = a.R + b.R; C = a.C + b.C }
      static member inline (-)(a : Vector2, b : Vector2) = { R = a.R - b.R; C = a.C - b.C }
      static member inline (*)(a : Vector2, b : 'a) = { R = a.R * int64 b; C = a.C * int64 b }
      static member inline (*)(a : 'a, b : Vector2) = { R = int64 a * b.R; C = int64 a * b.C }
      static member inline op_Implicit(v : Vector2i) : Vector2 = { R = int64 v.R; C = int64 v.C }
      static member inline Distance(a : Vector2, b : Vector2) = (b - a).Magnitude
      static member inline ManhattanDistance(a : Vector2, b : Vector2) = abs (b.C - a.C) + abs (b.R - a.R)
