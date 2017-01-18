module Geom

/// A class representing three-dimensional vectors. Supports a variety of
/// common vector operations.
type V3 = class
   
   /// The x component of the vector
   member X : float
   /// The y component of the vector
   member Y : float
   /// The z component of the vector
   member Z : float

   /// Constructs a vector using three coordinates
   new : x:float * y:float * z:float -> V3

   /// Adds two vectors
   static member (+) : v1:V3 * v2:V3 -> V3
   /// Scales a vector
   static member (*) : v1:V3 * s:float -> V3
   /// Scales a vector
   static member (*) : s:float * v1:V3 -> V3
   /// Scales a vector down by a factor
   static member (/) : v1:V3 * s:float -> V3
   /// Computes the dot product of two vectors
   static member (.*) : v1:V3 * v2:V3 -> float
   /// Negates a vector
   static member (~-) : v1:V3 -> V3
   /// Subtracts two vectors
   static member (-) : v1:V3 * v2:V3 -> V3

   /// The squared norm of the vector
   member Norm2 : float
   /// The norm of the vector
   member Norm : float
   /// The unit vector in the same direction as this vector, or zero
   /// if the vector is infinitesimal
   member Unit : V3

end