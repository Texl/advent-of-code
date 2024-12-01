namespace AdventOfCode.Common

[<AutoOpen>]
module Vector3Types =
    [<StructuredFormatDisplay("({X}, {Y}, {Z})")>]
    type Vector3 =
        { X : int64
          Y : int64
          Z : int64 }

        member this.Length =
           let x = this.X
           let y = this.Y
           let z = this.Z
           System.Math.Sqrt(float (x * x + y * y + z * z))
        
        static member Zero = { X = 0L; Y = 0L; Z = 0L }
        static member One = { X = 1L; Y = 1L; Z = 1L }
        static member UnitX = { X = 1L; Y = 0L; Z = 0L }
        static member UnitY = { X = 0L; Y = 1L; Z = 0L }
        static member UnitZ = { X = 0L; Y = 0L; Z = 1L }

        static member Init (x : int64, y : int64, z : int64) =
            { X = x
              Y = y
              Z = z }
        
        static member (~-) (a : Vector3) =
            { X = -a.X
              Y = -a.Y
              Z = -a.Z }

        static member (+) (a : Vector3, b : Vector3) =
            { X = a.X + b.X
              Y = a.Y + b.Y
              Z = a.Z + b.Z }

        static member (-) (a : Vector3, b : Vector3) =
            { X = a.X - b.X
              Y = a.Y - b.Y
              Z = a.Z - b.Z }

        static member (*) (a : int64, b : Vector3) =
            { X = a * b.X
              Y = a * b.Y
              Z = a * b.Z }

        static member (*) (a : Vector3, b : int64) =
            { X = a.X * b
              Y = a.Y * b
              Z = a.Z * b }

        static member (/) (a : Vector3, b : Vector3) =
            { X = a.X / b.X
              Y = a.Y / b.Y
              Z = a.Z / b.Z }
            
        static member Dot (a : Vector3, b : Vector3) =
            a.X * b.X + a.Y * b.Y + a.Z * b.Z


[<RequireQualifiedAccess>]
module Vector3 =
    let toTuple (v : Vector3) = v.X, v.Y, v.Z

    let minElements (a : Vector3) (b : Vector3) =
        { X = min a.X b.X
          Y = min a.Y b.Y
          Z = min a.Z b.Z }

    let maxElements (a : Vector3) (b : Vector3) =
        { X = max a.X b.X
          Y = max a.Y b.Y
          Z = max a.Z b.Z }
