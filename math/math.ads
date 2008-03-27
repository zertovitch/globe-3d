
with interfaces.C;

with ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;



package Math is
   --
   -- provides math.


   pragma pure;
   pragma optimize (Time);


   subtype Integer  is interfaces.c.Int;
   subtype Natural  is Integer range 0 .. Integer'Last;
   subtype Positive is Integer range 1 .. Integer'Last;

   procedure increment (Self : in out Integer;   By : in Integer := 1);
   procedure decrement (Self : in out Integer;   By : in Integer := 1);


   subtype Real         is interfaces.c.Double;
   subtype natural_Real is Real range 0.0 .. Real'last;


   function Clamped (Self : in Real;   Low  : in Real;
                                       High : in Real) return Real;


   type    Reals      is array (math.Positive range <>) of aliased Real;

   subtype Reals_1   is Reals (1 .. 1);
   subtype Reals_2   is Reals (1 .. 2);
   subtype Reals_3   is Reals (1 .. 3);
   subtype Reals_4   is Reals (1 .. 4);
   subtype Reals_5   is Reals (1 .. 5);
   subtype Reals_6   is Reals (1 .. 6);

   function Image (Self : in Reals) return String;



   type real_Block is array (Positive range <>, Positive range <>) of aliased Real;   -- tbd: better name ?

   subtype real_Block_3x3   is real_Block (1 .. 3,  1 .. 3);
   subtype real_Block_4x3   is real_Block (1 .. 4,  1 .. 3);
   subtype real_Block_6x3   is real_Block (1 .. 6,  1 .. 3);


   function to_Numbers (Self : in real_Block) return Reals;
   function Min        (Self : in real_Block) return Real;
   function Max        (Self : in real_Block) return Real;




   -- common constants
   --

   Infinity : constant Real := Real'last;
   Pi       : constant Real := ada.numerics.Pi;
   Phi      : constant Real := 1.618033988749895; -- tbd: more accurate


   package Functions is new Ada.Numerics.Generic_Elementary_Functions (Real);




   function to_Radians (Degrees : in math.Real) return math.Real;
   function to_Degrees (Radians : in math.Real) return math.Real;



   -- vector and matrix
   --

   type Vector is array (Integer range <>)                   of aliased Real;

   type Vector_2 is new Vector (1 .. 2);
   subtype Vector_3 is Vector (1 .. 3);
   subtype Vector_4 is Vector (1 .. 4);

   type vector_2_Array is array (math.Integer range <>) of aliased Vector_2;
   type vector_3_Array is array (math.Integer range <>) of Vector_3;





   type Matrix is array (Integer range <>, Integer range <>) of aliased Real;

   type Matrix_2x2 is new Matrix (1 .. 2, 1 .. 2);
   subtype Matrix_3x3 is Matrix (1 .. 3, 1 .. 3);
   subtype Matrix_4x4 is Matrix (1 .. 4, 1 .. 4);

   Identity_2x2 : aliased constant Matrix_2x2;
   Identity_3x3 : constant Matrix_3x3;



   type Quaternion is new Vector_4;



   type Transform_2d is
      record
         Position : aliased         Vector_2;
         Rotation : access constant Matrix_2x2;
         --Rotation : aliased Matrix_2x2;
      end record;








private


   Identity_2x2 : aliased constant Matrix_2x2 := ((1.0, 0.0),
                                                  (0.0, 1.0));

   Identity_3x3 : constant Matrix_3x3 := ((1.0, 0.0, 0.0),
                                          (0.0, 1.0, 0.0),
                                          (0.0, 0.0, 1.0));



   pragma inline_always (increment);
   pragma inline_always (decrement);

   pragma inline_always (Clamped);
end Math;


-- notes:
--
