

package math.Algebra.linear.d3 is

   pragma Pure;


   -- Vector_3
   --

   function "*" (Left : Vector_3;     Right : Number)   return Vector_3;       -- scale
   function "*" (Left : Number;       Right : Vector_3) return Vector_3;       -- scale

   function "*" (Left : Vector_3;     Right : Vector_3) return Number;         -- dot product
   function "*" (Left : Vector_3;     Right : Vector_3) return Vector_3;       -- cross product

   function "+" (Left : Vector_3;     Right : Vector_3) return Vector_3;
   function "-" (Left : Vector_3;     Right : Vector_3) return Vector_3;


   function Distance (Self : in Vector_3;   To : in Vector_3) return Number;

   function Midpoint              (Left, Right: Vector_3) return Vector_3;
   function angle_Between_preNorm (U : in Vector_3;   V : in Vector_3) return Number;




   -- matrix_3x3
   --

   function to_Matrix (Row_1, Row_2, Row_3 : in Vector_3) return Matrix_3x3;


   function "*" (Left : Matrix_3x3;   Right : Vector_3)     return Vector_3;
   function "*" (Left : Matrix_3x3;   Right : Matrix_3x3)   return Matrix_3x3;

   function forward_Direction (Self : in Matrix_3x3) return Vector_3;
   function up_Direction      (Self : in Matrix_3x3) return Vector_3;
   function right_Direction   (Self : in Matrix_3x3) return Vector_3;


   function Identity return Matrix_3x3;

   function x_Rotation_from (the_Angle : in Number) return Matrix_3x3;
   function y_Rotation_from (the_Angle : in Number) return Matrix_3x3;
   function z_Rotation_from (the_Angle : in Number) return Matrix_3x3;

   function xyz_Rotation (x_Angle, y_Angle, z_Angle : in Number) return math.Matrix_3x3;


   function to_Attitude (Axis_x, Axis_y, Axis_z   : in     Number;
                         rotation_Angle           : in     Number) return Matrix_3x3;
   --
   -- returns an attitude describing a rotation about an axis.



   -- quaternion
   --

   procedure setFromMatrix3x3T (Self :    out Quaternion;   the_Matrix : in Matrix_3x3);

   function to_Quaternion (Self : in Matrix_3x3) return Quaternion;
   function to_Matrix     (Self : in Quaternion) return Matrix_3x3;




private

   pragma inline ("+");
   pragma inline ("-");
   pragma inline ("*");

end math.Algebra.linear.d3;


-- notes:
--
