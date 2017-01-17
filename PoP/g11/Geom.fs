module Geom

type V3(x, y, z: float) = class
   
   member this.X = x
   member this.Y = y
   member this.Z = z

   override this.ToString () = sprintf "(%A, %A, %A)" x y z

   static member (+) (v1: V3, v2: V3) = V3(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v2.Z)
   static member (*) (v1: V3, s: float) = V3(v1.X * s, v1.Y * s, v1.Z * s)
   static member (*) (s: float, v1: V3) = V3(v1.X * s, v1.Y * s, v1.Z * s)
   static member (/) (v1: V3, s: float) = V3(v1.X / s, v1.Y / s, v1.Z / s)
   static member (.*) (v1: V3, v2: V3) = v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z
   static member (~-) (v: V3) = v * (-1.0)
   static member (-) (v1: V3, v2: V3) = v1 + (-v2)

   member this.Norm2 = this .* this
   member this.Norm = sqrt this.Norm2
   member this.Unit =
      let norm = this.Norm
      // return zero vector instead of NaN
      // when taking unit vector of zero
      if norm < 1E-20 then
         this
      else
         this / norm

end