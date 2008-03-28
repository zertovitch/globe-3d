
--with ada_BLAS.Real;

--with pragmarc.Matrix_math;

with ada.numerics.generic_real_arrays;

--with laCmp;



package body math.Algebra.linear.d3 is


   use math.Functions;
   use type Real, Integer;



   -- BLAS
   --

--   package BLAS is new ada_BLAS.Real (math.Number, math.Integer, math.Vector, math.Matrix);



   -- Vector_3
   --

--     function Normalised (Self : in Vector_3) return Vector_3
--     is
--        the_Norm : Number := Norm (Self);
--     begin
--        return (Self (1) / the_Norm,
--                Self (2) / the_Norm,
--                Self (3) / the_Norm);
--     end;



   function "*" (Left, Right : Vector_3) return Vector_3
   is
   begin
      return (1 => Left (2) * Right (3)  -  Left (3) * Right (2),
              2 => Left (3) * Right (1)  -  Left (1) * Right (3),
              3 => Left (1) * Right (2)  -  Left (2) * Right (1));
   end;




   function "*" (Left : Vector_3;     Right : Vector_3) return Real
   is
   begin
      return   Left (1) * Right (1)
             + Left (2) * Right (2)
             + Left (3) * Right (3);
   end;





   function angle_Between_preNorm (U : in Vector_3;   V : in Vector_3) return math.Real
   is
      use math.Functions;
      Val : math.Real := U * V;   -- dot product
   begin
      if    val < -1.0 then   val := -1.0;   -- clamp to avoid rounding errors; acos will fail with values outside this range.
      elsif val >  1.0 then   val :=  1.0;
      end if;

      return arcCos (Val);
   end;




   function Midpoint (Left, Right: Vector_3) return Vector_3
   is
   begin
      return ((Left (1) + Right (1)) * 0.5,
              (Left (2) + Right (2)) * 0.5,
              (Left (3) + Right (3)) * 0.5);
   end;




   function Distance (Self : in Vector_3;   To : in Vector_3) return Real
   is
      Pad : Vector := Self - To;
   begin
      return Norm (Pad);
   end;




   function "+" (Left, Right : Vector_3) return Vector_3
   is
   begin
      return (Left (1) + Right (1),
              Left (2) + Right (2),
              Left (3) + Right (3));
   end;



   function "-" (Left : Vector_3;     Right : Vector_3) return Vector_3
   is
   begin
      return (Left (1) - Right (1),
              Left (2) - Right (2),
              Left (3) - Right (3));
   end;



   function "*" (Left : Vector_3;     Right : Real)   return Vector_3
   is
   begin
      return (Left (1) * Right,
              Left (2) * Right,
              Left (3) * Right);
   end;



   function "*" (Left : Real;       Right : Vector_3) return Vector_3
   is
   begin
      return Right * Left;
   end;





   -- matrix_3x3
   --

   function to_Matrix (Row_1, Row_2, Row_3 : in Vector_3) return Matrix_3x3
   is
   begin
      return ((Row_1 (1), Row_1 (2), Row_1 (3)),
              (Row_2 (1), Row_2 (2), Row_2 (3)),
              (Row_3 (1), Row_3 (2), Row_3 (3)));

   end;





   function forward_Direction (Self : in Matrix_3x3) return Vector_3
   is
   begin
      return Self * Vector_3'((0.0, 0.0, 1.0));
   end;





   function up_Direction (Self : in Matrix_3x3) return Vector_3
   is
   begin
      return Self * Vector_3'((0.0, 1.0, 0.0));
   end;



   function right_Direction (Self : in Matrix_3x3) return Vector_3
   is
   begin
      return Self * Vector_3'((1.0, 0.0, 0.0));
   end;






   -- pragmarc
   --

--     package pragmarc_Matrices is new pragmarc.Matrix_math (Real, -1.0, 0.0);
--

--     subtype pragmarc_Vector   is pragmarc_Matrices.Vector;
--     subtype pragmarc_Vector_3 is pragmarc_Matrices.Vector (3);
--     subtype pragmarc_Vector_4 is pragmarc_Matrices.Vector (4);
--
--     subtype pragmarc_Matrix   is pragmarc_Matrices.Matrix;
--     subtype pragmarc_Matrix_3 is pragmarc_Matrices.Matrix (3, 3);
--
--     use pragmarc_Matrices;
--     use type pragmarc_Matrix_3;





--     function to_Pragmarc (Self : in Quaternion) return pragmarc_Vector
--     is
--        the_Vector : pragmarc_Vector (4);
--     begin
--        for Each in 1 .. 4 loop
--           the_vector.value.Value (each, 1) := Self (math.Integer (Each));
--        end loop;
--
--        return the_Vector;
--     end;





--     function to_Math (Self : in pragmarc_Vector_4) return Quaternion
--     is
--        the_Quaternion : Quaternion;
--     begin
--        for Each in 1 .. 4 loop
--            the_Quaternion (math.Integer (Each)) := Self.value.value (Each, 1);
--        end loop;
--
--        return the_Quaternion;
--     end;










--     function to_pragmarc (Self : in Matrix_3x3) return pragmarc_Matrix
--     is
--        the_Matrix : pragmarc_Matrix (self'Length (1), self'Length (2));
--     begin
--        for Row in self'Range (1) loop
--           for Col in self'Range (2) loop
--              the_Matrix.value (standard.Integer (Row), standard.Integer (Col)) := Self (Row, Col);
--           end loop;
--        end loop;
--
--        return the_Matrix;
--     end;





--     function to_Math (Self : in pragmarc_Matrix_3) return Matrix_3x3
--     is
--        the_Matrix : Matrix_3x3;
--     begin
--        for Row in 1 .. self.num_Rows loop
--           for Col in 1 .. self.num_Columns loop
--               the_Matrix (math.Integer (Row), math.Integer (Col)) := self.Value (Row, Col);
--           end loop;
--        end loop;
--
--        return the_Matrix;
--     end;






   function Identity return Matrix_3x3
   is
   begin
      return ((1.0, 0.0, 0.0),
              (0.0, 1.0, 0.0),
              (0.0, 0.0, 1.0));
   end;






   function "*" (Left : Matrix_3x3;   Right : Vector_3) return Vector_3
   is
   begin
      return (left (1, 1) * right (1)  +  left (1, 2) * right (2)  +  left (1, 3) * right (3),
              left (2, 1) * right (1)  +  left (2, 2) * right (2)  +  left (2, 3) * right (3),
              left (3, 1) * right (1)  +  left (3, 2) * right (2)  +  left (3, 3) * right (3));
   end;



   function "*" (Left  : Matrix_3x3;   Right : Matrix_3x3) return Matrix_3x3
   is
      r  : Real;
      AB : Matrix_3x3;
   begin
      for i in Left'first (1) .. Left'last (1) loop
         for j in Left'first (2) .. Left'last (2) loop
            r := 0.0;
            for k in Left'first (1) .. Left'last (1) loop
               r := r  +  (Left (i, k)  *  Right (k, j));
            end loop;
            AB (i, j) := r;
         end loop;
      end loop;

      return AB;
   end "*";



   function xyz_Rotation (x_Angle, y_Angle, z_Angle : in Real) return math.Matrix_3x3
   is
   begin
      return x_Rotation_from (x_Angle) * y_Rotation_from (y_Angle) * z_Rotation_from (z_Angle);
   end;





   function x_Rotation_from (the_Angle : in Real) return Matrix_3x3
   is
      the_Matrix : Matrix_3x3;
   begin
      the_matrix (1, 1) := 1.0;
      the_matrix (1, 2) := 0.0;
      the_matrix (1, 3) := 0.0;

      the_matrix (2, 1) := 0.0;
      the_matrix (2, 2) := cos (the_Angle);
      the_matrix (2, 3) := -sin (the_Angle);

      the_matrix (3, 1) := 0.0;
      the_matrix (3, 2) := sin (the_Angle);
      the_matrix (3, 3) := cos (the_Angle);

      return the_Matrix;
   end;






   function y_Rotation_from (the_Angle : in Real) return Matrix_3x3
   is
      the_Matrix : Matrix_3x3;
   begin
      the_Matrix (1, 1) := cos (the_Angle);
      the_Matrix (1, 2) := 0.0;
      the_Matrix (1, 3) := -sin (the_Angle);

      the_Matrix (2, 1) := 0.0;
      the_Matrix (2, 2) := 1.0;
      the_Matrix (2, 3) := 0.0;

      the_Matrix (3, 1) := sin (the_Angle);
      the_Matrix (3, 2) := 0.0;
      the_Matrix (3, 3) := cos (the_Angle);

      return the_Matrix;
   end;





   function z_Rotation_from (the_Angle : in Real) return Matrix_3x3
   is
      the_Matrix : Matrix_3x3;
   begin
      the_Matrix (1, 1) := cos (the_Angle);
      the_Matrix (1, 2) := -sin (the_Angle);
      the_Matrix (1, 3) := 0.0;

      the_Matrix (2, 1) := sin (the_Angle);
      the_Matrix (2, 2) := cos (the_Angle);
      the_Matrix (2, 3) := 0.0;

      the_Matrix (3, 1) := 0.0;
      the_Matrix (3, 2) := 0.0;
      the_Matrix (3, 3) := 1.0;

      return the_Matrix;
   end;







   function to_Attitude (Axis_x, Axis_y, Axis_z   : in     Real;
                         rotation_Angle           : in     Real) return Matrix_3x3
   is
   begin
      return to_Matrix (to_Quaternion (Axis_x, Axis_y, Axis_z, rotation_Angle));
   end;






   ------------------------------------------------------------------------------------------------------------------------------
   -- Quaternions
   --

   function to_Quaternion (Self : in Matrix_3x3) return Quaternion
   is
      TR : Real;
      S  : Real;

      the_Quaternion : Quaternion;
   begin
      TR := Self (1, 1)  +  Self (2, 2)  +  Self (3, 3);

      if TR >= 0.0 then
         S                  := Sqrt (TR + 1.0);
         the_Quaternion (1) := 0.5 * S;

         S                  := 0.5 * (1.0 / S);
         the_Quaternion (2) := (Self (3, 2)  -  Self (2, 3)) * S;
         the_Quaternion (3) := (Self (1, 3)  -  Self (3, 1)) * S;
         the_Quaternion (4) := (Self (2, 1)  -  Self (1, 2)) * S;

         return the_Quaternion;
      end if;


      -- otherwise, find the largest diagonal element and apply the appropriate case.
      --
      declare

         function case_1_Result return Quaternion
         is
         begin
            S                  := Sqrt (Self (1, 1)  -  (Self (2, 2)  +  Self (3, 3))  +  1.0);
            the_Quaternion (2) := 0.5 * S;

            S                  := 0.5 * (1.0 / S);
            the_Quaternion (3) := (Self (1, 2) + Self (2, 1)) * S;
            the_Quaternion (4) := (Self (3, 1) + Self (1, 3)) * S;
            the_Quaternion (1) := (Self (3, 2) - Self (2, 3)) * S;

            return the_Quaternion;
         end;


         function case_2_Result return Quaternion
         is
         begin
            S                  := Sqrt (Self (2, 2)  -  (Self (3, 3)  +  Self (1, 1))  +  1.0);
            the_Quaternion (3) := 0.5 * S;

            S                  := 0.5 * (1.0 / S);
            the_Quaternion (4) := (Self (2, 3) + Self (3, 2)) * S;
            the_Quaternion (2) := (Self (1, 2) + Self (2, 1)) * S;
            the_Quaternion (1) := (Self (1, 3) - Self (3, 1)) * S;

            return the_Quaternion;
         end;


         function case_3_Result return Quaternion
         is
         begin
            S                  := Sqrt (Self (3, 3)  -  (Self (1, 1) + Self (2, 2))  +  1.0);
            the_Quaternion (4) := 0.5 * S;

            S                  := 0.5 * (1.0 / S);
            the_Quaternion (2) := (Self (3, 1) + Self (1, 3)) * S;
            the_Quaternion (3) := (Self (2, 3) + Self (3, 2)) * S;
            the_Quaternion (1) := (Self (2, 1) - Self (1, 2)) * S;

            return the_Quaternion;
         end;


         pragma inline (case_1_Result);
         pragma inline (case_2_Result);
         pragma inline (case_3_Result);

      begin

         if Self (2, 2) > Self (1, 1) then

            if Self (3, 3) > Self (2, 2) then
               return case_3_Result;
            end if;

            return case_2_Result;
         end if;

         if Self (3, 3) > Self (1, 1) then
            return case_3_Result;
         end if;

         return case_1_Result;

      end;


   end;




   procedure setFromMatrix3x3T (Self :    out Quaternion;   the_Matrix : in Matrix_3x3)
   is
      tr : math.Real := 1.0 + the_Matrix (1, 1) + the_Matrix (2, 2) + the_Matrix (3, 3);
      s  : math.Real;
   begin
      if tr > 1.0e-9 then
         s        := sqRt (tr);
         Self (1) := 0.5 * s;
         s        := 0.5 / s;
         Self (2) := (the_Matrix (2, 3) - the_Matrix (3, 2)) * s;
         Self (3) := (the_Matrix (3, 1) - the_Matrix (1, 3)) * s;
         Self (4) := (the_Matrix (1, 2) - the_Matrix (2, 1)) * s;
      else
         declare
            I : Integer := 0;
         begin
            if the_Matrix (2, 2) > the_Matrix (1, 1) then
               I := 1;
            end if;

            if the_Matrix (3, 3) > the_Matrix (i, i) then
               I := 2;
            end if;

            case I is
               when 0 =>
                  s        := sqrt ((the_Matrix (1, 1) - (the_Matrix (2, 2) + the_Matrix (3, 3))) + 1.0);
                  Self (2) := 0.5 * s;
                  s        := 0.5 / s;
                  Self (3) := (the_Matrix (2, 1) + the_Matrix (1, 2)) * s;
                  Self (4) := (the_Matrix (1, 3) + the_Matrix (3, 1)) * s;
                  Self (1) := (the_Matrix (2, 3) - the_Matrix (3, 2)) * s;

               when 1 =>
                  s        := sqrt ((the_Matrix (2, 2) - (the_Matrix (3, 3) + the_Matrix (1, 1))) + 1.0);
                  Self (3) := 0.5 * s;
                  s        := 0.5 / s;
                  Self (4) := (the_Matrix (3, 2) + the_Matrix (2, 3)) * s;
                  Self (2) := (the_Matrix (2, 1) + the_Matrix (1, 2)) * s;
                  Self (1) := (the_Matrix (3, 1) - the_Matrix (1, 3)) * s;

               when 2 =>
                  s        := sqrt ((the_Matrix (3, 3) - (the_Matrix (1, 1) + the_Matrix (2, 2))) + 1.0);
                  Self (4) := 0.5 * s;
                  s        := 0.5 / s;
                  Self (2) := (the_Matrix (1, 3) + the_Matrix (3, 1)) * s;
                  Self (3) := (the_Matrix (3, 2) + the_Matrix (2, 3)) * s;
                  Self (1) := (the_Matrix (1, 2) - the_Matrix (2, 1)) * s;

               when others =>
                  raise program_Error;
            end case;
         end;

      end if;

   end;




   function to_Matrix (Self : in Quaternion) return Matrix_3x3
   is
      the_Matrix : Matrix_3x3;

      qq2 : Real := 2.0  *  Self (2)  *  Self (2);
      qq3 : Real := 2.0  *  Self (3)  *  Self (3);
      qq4 : Real := 2.0  *  Self (4)  *  Self (4);
   begin
      the_Matrix (1, 1) := 1.0 - qq3 - qq4;
      the_Matrix (1, 2) := 2.0 * (Self (2) * Self (3) - Self (1) * Self (4));
      the_Matrix (1, 3) := 2.0 * (Self (2) * Self (4) + Self (1) * Self (3));

      the_Matrix (2, 1) := 2.0 * (Self (2) * Self (3) + Self (1) * Self (4));
      the_Matrix (2, 2) := 1.0 - qq2 - qq4;
      the_Matrix (2, 3) := 2.0 * (Self (3) * Self (4) - Self (1) * Self (2));

      the_Matrix (3, 1) := 2.0 * (Self (2) * Self (4) - Self (1) * Self (3));
      the_Matrix (3, 2) := 2.0 * (Self (3) * Self (4) + Self (1) * Self (2));
      the_Matrix (3, 3) := 1.0 - qq2 - qq3;

      return the_Matrix;
   end;



end math.Algebra.linear.d3;


-- notes:
--
