
with math.fast_Trigonometry;
with math.fast_Rotation;



generic
   with package fast_Rot is new math.fast_Rotation;
package math.Algebra.linear.d2 is

   --pragma Pure;
   pragma optimize (Time);


   -- Vector_2
   --

   function "-" (Self : Vector_2)                       return Vector_2;
   function "-" (Left : Vector_2;     Right : Vector_2) return Vector_2;
   function "+" (Left : Vector_2;     Right : Vector_2) return Vector_2;

   function "*" (Left : Vector_2;     Right : Vector_2) return Real;   -- dot product

   function "*" (Left : Vector_2;     Right : Real)   return Vector_2;
   function "*" (Left : Real;         Right : Vector_2) return Vector_2;

   function min (Left : Vector_2;     Right : Vector_2) return Vector_2;
   function max (Left : Vector_2;     Right : Vector_2) return Vector_2;

   --   function Norm2 (Self : Vector_2) return math.Number;

   function Cross (Left  : in Vector_2;   Right : in Vector_2) return Real;
   function Cross (Self  : in Vector_2;   Scale : in Real)   return Vector_2;
   function Cross (Scale : in Real;     Self  : in Vector_2) return Vector_2;

   function Centroid (Self : Vector_2_Array) return Vector_2;

   function Normalise (Self : access Vector_2) return Real;
   --
   -- normalises 'Self' and returns the normal length.

   procedure normalise  (Self : in out Vector_2);
   function  Normalised (Self : in     Vector_2) return Vector_2;

   function Norm   (Self : in Vector_2) return Real;           -- the length or magnitude of the vector
   function Norm_2 (Self : in Vector_2) return Real;           -- the length or magnitude of the vector squared


   function Clamped (Self : in Vector_2;   Low  : in Vector_2;
                                           High : in Vector_2) return Vector_2;



   -- Matrix_2x2
   --

   function to_Rotation (Angle : in Real) return access constant Matrix_2x2;
--   function to_Rotation (Angle : in Real) return Matrix_2x2;

--   procedure set_Rotation (Self : access Matrix_2x2;   the_Angle : in Number);


--   function "*"         (Left : Matrix_2x2;       Right : Vector_2)       return Vector_2;

--   function "*"         (Left : Matrix_2x2;       Right : Matrix_2x2)     return Matrix_2x2;

   function "abs" (Self : in Matrix_2x2) return Matrix_2x2;


   function Solve (Self : in Matrix_2x2;   b : in Vector_2) return Vector_2;
   --
   -- Solve A * x = b

   function mul   (Left : access constant Matrix_2x2;       Right : access Matrix_2x2)     return Matrix_2x2;
   function mul_T (Left : access constant Matrix_2x2;       Right : access Matrix_2x2)     return Matrix_2x2;

   function mul   (Left : access constant Matrix_2x2;   Right : access Vector_2) return Vector_2;
   function mul_T (L    : access constant Matrix_2x2;   R     : access Vector_2) return Vector_2;    -- box2d



   -- Transform
   --

   --function "*" (L : Transform_2d;   R : Vector_2) return Vector_2;
   function mul   (L : access constant Transform_2d;   R : access Vector_2) return Vector_2;     -- box2d
   function mul_T (L : access constant Transform_2d;   R : access Vector_2) return Vector_2;     -- box2d

   function Image (Self : in Transform_2d) return String;



private

   pragma inline_always ("+");
   pragma inline_always ("-");
   pragma inline_always ("*");
   pragma inline_always ("abs");

   pragma inline_always (mul);
   pragma inline_always (mul_T);

   pragma inline_always (max);
   pragma inline_always (min);

   pragma inline_always (clamped);
--   pragma inline_always (centroid);

   pragma inline_always (normalise);
   pragma inline_always (normalised);
   pragma inline_always (norm);
   pragma inline_always (norm_2);

   pragma inline_always (cross);

   pragma inline_always (to_Rotation);
--   pragma inline_always (set_Rotation);

end math.Algebra.linear.d2;


-- notes:
--
