
generic
package math.Algebra.linear is

   pragma Pure;
   pragma optimize (Time);


   -- vector
   --

   function  Norm_2     (Self : in     Vector) return Real;   -- length squared
   function  Norm       (Self : in     Vector) return Real;   -- magnitude or length
   function  Normalised (Self : in     Vector) return Vector;
   procedure Normalise  (Self : in out Vector);

   procedure scale        (Self : in out Vector;   By : Real);
   function  sum_Abs      (Self : in     Vector) return Real;           -- sum of absoluted components.
   function  Index_of_max (Self : in     Vector) return Integer;     -- returns the index to the maximum component.
--   function  to_Degrees   (Self : in     Vector) return Vector;

   procedure swap (X, Y : in out Vector);
   procedure copy (From : in     Vector;
                   To   : in out Vector);

--   function "*" (Left : Vector;       Right : Vector)   return Number;         -- dot product
--   function "*" (Left : Vector;       Right : Number)   return Vector;
--   function "*" (Left : Number;       Right : Vector)   return Vector;
   function "/" (Left : Vector;       Right : Real)   return Vector;
--   function "+" (Left : Vector;       Right : Vector)   return Vector;

--   function Min (Left : in Vector;   Right : in Vector) return Vector;
--   function Max (Left : in Vector;   Right : in Vector) return Vector;


   function Image (Self : in Vector) return String;



   -- matrix
   --

   function to_Matrix (Row_1, Row_2, Row_3 : in Vector_3) return Matrix_3x3;

   function Min        (Self : in Matrix) return Real;
   function Max        (Self : in Matrix) return Real;


--     function sub_Matrix (Self : in Matrix;   start_Row, end_Row : in Index_type;
--                                              start_Col, end_Col : in Index_type) return math.Matrix;
   function sub_Matrix (Self : in Matrix;   start_Row, end_Row : in Integer;
                                            start_Col, end_Col : in Integer) return math.Matrix;

   --function Transposed (Self : in Matrix) return math.Matrix;

   function Image (Self : in Matrix) return String;


   -- inversion
   --

   --procedure invert (Self : in out Matrix);

--   procedure invert_positive_definite (Self : in out Matrix);
   --
   -- uses lapack DPOTRI

--   function  Inverted_positive_definite (Self : in Matrix) return Matrix;
   --
   --  return the inverse of the square positive definite matrix 'Self'.
   --  raises constraint_Error when 'Self' is not a positive definite



   function "*" (Left : Matrix;       Right : Real)       return Matrix;
   --function "*" (Left : Matrix_3x3;       Right : Matrix_3x3)       return Matrix;



--   function Identity (Length : in Index_type := 3) return Matrix;
   function Identity (Length : in Integer := 3) return Matrix;



   -- quaternion
   --

   function to_Quaternion (aX, aY, aZ : in     Real;
                           Angle      : in     Real) return Quaternion;
   --
   -- defines self as a rotation about an axis.


   function "*"           (Self : in     Quaternion;   By    : in    Quaternion) return Quaternion;        -- Grassmann product
   function  Unit         (Self : in     Quaternion)                             return Quaternion;
   function  Conjugate    (Self : in     Quaternion)                             return Quaternion; -- tbd: only for unit quaternions.
   function  euler_Angles (Self : in     Quaternion)                             return Vector_3;


   function infinitesimal_Rotation_from (Self : in     Quaternion;   angular_Velocity : in Vector_3) return Quaternion;
   --
   -- an infinitesimal_Rotation, may be multiplied by a duration, and added to the original Attitude, to
   -- produce an updated Attitude.


   function  Normalised (Self : in     Quaternion) return Quaternion;
   procedure normalise  (Self : in out Quaternion);




private

   pragma inline_always ("*");
--   pragma inline ("+");
   pragma inline_always (swap);
   pragma inline_always (copy);
   pragma inline_always (scale);

   pragma inline_always (Norm_2);
   pragma inline_always (Norm);
   pragma inline_always (Normalise);

   pragma inline_always (scale);
   pragma inline_always (sum_Abs);
   pragma inline_always (Index_of_max);
--   pragma inline (to_Degrees);

end math.Algebra.linear;


-- notes:
--
-- - use platform optimised BLAS library for best performance (ie ATLAS, etc).
