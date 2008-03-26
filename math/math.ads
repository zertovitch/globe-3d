
--with laBase;
with interfaces.Fortran;

with ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Generic_Real_Arrays;



package Math is -- provides math.

   pragma pure;
   pragma optimize (Time);


   subtype Integer  is interfaces.fortran.fortran_Integer;
   subtype Natural  is Integer range 0 .. Integer'Last;
   subtype Positive is Integer range 1 .. Integer'Last;

   procedure increment (Self : in out Integer;   By : in Integer := 1);
   procedure decrement (Self : in out Integer;   By : in Integer := 1);


   -- subtype Number         is long_Float;
   subtype Number         is interfaces.fortran.double_Precision;
   subtype natural_Number is Number range 0.0 .. Number'last;


   function Clamped (Self : in Number;   Low  : in Number;
                                         High : in Number) return Number;


   type    Numbers      is array (math.Positive range <>) of aliased Number;

   subtype Numbers_1   is Numbers (1 .. 1);
   subtype Numbers_2   is Numbers (1 .. 2);
   subtype Numbers_3   is Numbers (1 .. 3);
   subtype Numbers_4   is Numbers (1 .. 4);
   subtype Numbers_5   is Numbers (1 .. 5);
   subtype Numbers_6   is Numbers (1 .. 6);

   function Image (Self : in Numbers) return String;



   type number_Block is array (Positive range <>, Positive range <>) of aliased Number;   -- tbd: better name ?

   subtype number_Block_3x3   is number_Block (1 .. 3,  1 .. 3);
   subtype number_Block_4x3   is number_Block (1 .. 4,  1 .. 3);
   subtype number_Block_6x3   is number_Block (1 .. 6,  1 .. 3);


   function to_Numbers (Self : in number_Block) return Numbers;
   function Min        (Self : in number_Block) return Number;
   function Max        (Self : in number_Block) return Number;




   -- common constants
   --

   Infinity : constant Number := Number'last;
   Pi       : constant Number := ada.numerics.Pi;
   Phi      : constant Number := 1.618033988749895; -- tbd: more accurate


   package Functions is new Ada.Numerics.Generic_Elementary_Functions (Number);




   function to_Radians (Degrees : in math.Number) return math.Number;
   function to_Degrees (Radians : in math.Number) return math.Number;



   -- vector and matrix
   --


--   package number_Arrays is new Ada.Numerics.Generic_Real_Arrays (Number);

--   type Vector is new number_arrays.real_Vector;

   --subtype Vector is laBase.fortran_double_precision_Vector;
   type Vector is array (Integer range <>)                   of aliased Number;

   --subtype Vector_2 is Vector (1 .. 2);
   type Vector_2 is new Vector (1 .. 2);
   subtype Vector_3 is Vector (1 .. 3);
   subtype Vector_4 is Vector (1 .. 4);

   type vector_2_Array is array (math.Integer range <>) of aliased Vector_2;
   type vector_3_Array is array (math.Integer range <>) of Vector_3;





   --subtype Matrix is laBase.fortran_double_precision_Matrix;
   type Matrix is array (Integer range <>, Integer range <>) of aliased Number;

   --subtype Matrix_2x2 is Matrix (1 .. 2, 1 .. 2);
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


--        procedure Opaque_view_write (Stream : access Ada.Streams.Root_Stream_Type'Class;
--                                     Self   : in     Opaque_view);
--        for Opaque_view'Write use Opaque_view_write;
--
--
--        procedure Opaque_view_read (Stream : access Ada.Streams.Root_Stream_Type'Class;
--                                    Self   : out   Opaque_view);
--        for Opaque_view'Read use Opaque_view_read;
--



   pragma inline_always (increment);
   pragma inline_always (decrement);

   pragma inline_always (Clamped);
end Math;


-- notes:
--
