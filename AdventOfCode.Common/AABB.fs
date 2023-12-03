namespace AdventOfCode.Common

[<AutoOpen>]
module AABBTypes =
   type AABB =
        { Min : Vector2
          Max : Vector2 }
      with
      member self.Contains (p : Vector2) =
         p.R >= self.Min.R &&
         p.R <= self.Max.R &&
         p.C >= self.Min.C &&
         p.C <= self.Max.C

[<RequireQualifiedAccess>]
module AABB =
   let contains (p : Vector2) (aabb : AABB) =
      aabb.Contains p
