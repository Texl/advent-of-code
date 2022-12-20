namespace AdventOfCode.Common

[<AutoOpen>]
module Vector4Types =
    type Vector4 =
        { W : int64
          X : int64
          Y : int64
          Z : int64 }

        member this.Item n =
            match n with
            | 0 -> this.W
            | 1 -> this.X
            | 2 -> this.Y
            | 3 -> this.Z
            | _ -> failwith "Invalid index"
            
        static member Zero = { W = 0L; X = 0L; Y = 0L; Z = 0L }
        static member One = { W = 1L; X = 1L; Y = 1L; Z = 1L }
        static member UnitW = { W = 1L; X = 1L; Y = 0L; Z = 0L }
        static member UnitX = { W = 0L; X = 1L; Y = 0L; Z = 0L }
        static member UnitY = { W = 0L; X = 0L; Y = 1L; Z = 0L }
        static member UnitZ = { W = 0L; X = 0L; Y = 0L; Z = 1L }

        static member Init (w : int64, x : int64, y : int64, z : int64) =
            { W = w
              X = x
              Y = y
              Z = z }
        
        static member (+) (a : Vector4, b : Vector4) =
            { W = a.W + b.W
              X = a.X + b.X
              Y = a.Y + b.Y
              Z = a.Z + b.Z }

        static member (-) (a : Vector4, b : Vector4) =
            { W = a.W - b.W
              X = a.X - b.X
              Y = a.Y - b.Y
              Z = a.Z - b.Z }

[<RequireQualifiedAccess>]
module Vector4 =
    let toTuple (v : Vector4) = v.W, v.X, v.Y, v.Z

    let minElements (a : Vector4) (b : Vector4) =
        { W = min a.W b.W
          X = min a.X b.X
          Y = min a.Y b.Y
          Z = min a.Z b.Z }

    let maxElements (a : Vector4) (b : Vector4) =
        { W = max a.W b.W
          X = max a.X b.X
          Y = max a.Y b.Y
          Z = max a.Z b.Z }
