namespace AdventOfCode.Common

[<AutoOpen>]
module Vector3Types =
    type Vector3 =
        { X : int64
          Y : int64
          Z : int64 }

        static member Zero = { X = 0L; Y = 0L; Z = 0L }
        static member One = { X = 1L; Y = 1L; Z = 1L }
        static member UnitX = { X = 1L; Y = 0L; Z = 0L }
        static member UnitY = { X = 0L; Y = 1L; Z = 0L }
        static member UnitZ = { X = 0L; Y = 0L; Z = 1L }

        static member Init (x : int64, y : int64, z : int64) =
            { X = x
              Y = y
              Z = z }
        
        static member (+) (a : Vector3, b : Vector3) =
            { X = a.X + b.X
              Y = a.Y + b.Y
              Z = a.Z + b.Z }

        static member (-) (a : Vector3, b : Vector3) =
            { X = a.X - b.X
              Y = a.Y - b.Y
              Z = a.Z - b.Z }

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
