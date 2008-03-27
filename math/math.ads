
with interfaces.C;

with ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;


generic
   type Integer_Type is range  <>;
   type Float_Type   is digits <>;

   type Index_Type   is range <>;
   vector_Base : Index_type;           -- specifiy '0' or '1' based indexing

   type Vector_Type is array (Index_Type range <>) of aliased Float_Type'Base;

   --  Vector_Type may need to be of convention Fortran:     -- tbd:
   --     pragma Convention (Fortran, Vector_Type);

   type Matrix_Type is array (Index_Type range <>, Index_Type range <>) of aliased Float_Type'Base;

   --  Matrix_Type MUST be of convention Fortran (column-major order):    -- tbd:
   --     pragma Convention (Fortran, Matrix_Type);


package Math is           -- tbd: rename to 'Adam' (for Ada Math) ?
   --
   -- provides math.


   pragma pure;
   pragma optimize (Time);


   subtype Integer  is Integer_type;
   subtype Natural  is Integer range 0 .. Integer'Last;
   subtype Positive is Integer range 1 .. Integer'Last;

   procedure increment (Self : in out Integer;   By : in Integer := 1);
   procedure decrement (Self : in out Integer;   By : in Integer := 1);


   subtype Real         is Float_type;
   subtype natural_Real is Real range 0.0 .. Real'last;


   function Clamped (Self : in Real;   Low  : in Real;
                                       High : in Real) return Real;




   package Functions is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use Functions;




   -- common constants
   --

   Infinity : constant Real := Real'last;
   Pi       : constant Real := ada.numerics.Pi;
   Phi      : constant Real := (sqrt (5.0) + 1.0) / 2.0;    -- the 'Golden' ratio




   -- angles
   --

   function to_Radians (Degrees : in math.Real) return math.Real;   -- tbd: define an Angle (sub)type ?
   function to_Degrees (Radians : in math.Real) return math.Real;





   -- vector and matrix   (see 'math.Algebra.linear' for subprograms)
   --

   subtype Vector is Vector_type;

   type    Vector_2 is new Vector (vector_Base .. vector_Base + 1);      -- tbd: resolve type/subtype inconsistency
   subtype Vector_3 is     Vector (vector_Base .. vector_Base + 2);
   subtype Vector_4 is     Vector (vector_Base .. vector_Base + 3);

   type vector_2_Array is array (Index_type range <>) of aliased Vector_2;
   type vector_3_Array is array (Index_type range <>) of aliased Vector_3;



   type Matrix is array (Index_type range <>, Index_type range <>) of aliased Real;

   type    Matrix_2x2 is new Matrix (vector_Base .. vector_Base + 1,   vector_Base .. vector_Base + 1);
   subtype Matrix_3x3 is     Matrix (vector_Base .. vector_Base + 2,   vector_Base .. vector_Base + 2);
   subtype Matrix_4x4 is     Matrix (vector_Base .. vector_Base + 3,   vector_Base .. vector_Base + 3);


   Identity_2x2 : aliased constant Matrix_2x2;
   Identity_3x3 :         constant Matrix_3x3;



   -- Quaternion
   --

   type Quaternion is new Vector_4;       -- tbd: use better abstraction (separate 'vector' component out).



   -- Transforms (Position + Rotation)
   --

   type Transform_2d is
      record
         Position : aliased         Vector_2;
         Rotation : access constant Matrix_2x2;       -- tbd: do performance test to see if 'access' is actually faster.
         --Rotation : aliased Matrix_2x2;
      end record;







private


   Identity_2x2 : aliased constant Matrix_2x2 := ((1.0, 0.0),
                                                  (0.0, 1.0));

   Identity_3x3 :         constant Matrix_3x3 := ((1.0, 0.0, 0.0),
                                                  (0.0, 1.0, 0.0),
                                                  (0.0, 0.0, 1.0));



   pragma inline_always (increment);
   pragma inline_always (decrement);

   pragma inline_always (Clamped);
end Math;


-- notes:
--
