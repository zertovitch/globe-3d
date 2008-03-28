
--with ada_BLAS.Real;

--with pragmarc.Matrix_math;
--with gsl_matrixs.Binding;                 --tbs: add pragma pure to swig generated files !!
--with gsl_linear_algebra.Binding;
--with ada_blas.Real;

with ada.numerics.generic_real_arrays;

--with laCmp;


--with math.Algebra.linear.d3;   use math.Algebra.linear.d3;  -- only for quaternion which will soon be moving out.

with ada.Characters.latin_1;



package body math.Algebra.linear is

   -- tbd: factor 'quaternion' out into it's own package.

   use math.Functions;
   use type math.Real, Integer;



   -- BLAS
   --

   --package BLAS is new ada_BLAS.Real (math.Number, math.Integer, math.Vector, math.Matrix);




   -- vectors
   --

   -- tbd: add remaining BLAS operations.


   function Norm_2 (Self : in Vector) return Real
   is
      the_Norm_2 : Real := 0.0;
   begin
      for Each in Self'range loop
         the_Norm_2 := the_Norm_2 + Self (Each) * Self (Each);
      end loop;


      -- return blas.NRM2 (Self);
      return the_Norm_2;
   end;



   function Norm (Self : in Vector) return Real
   is
   begin
      return Sqrt (Norm_2 (Self));
      --return blas.NRM2 (Self);
   end;



   procedure normalise (Self : in out Vector)
   is
     inv_Norm : Real := 1.0 / Norm (Self);
   begin
      for Each in self'Range loop
         Self (Each) := Self (Each) * inv_Norm;
      end loop;
   end;



   function Normalised (Self : in Vector) return Vector
   is
      Result : Vector := Self;
   begin
      normalise (Result);
      return Result;
   end;





   function sum_Abs (Self : in     Vector) return Real
   is
   begin
      raise constraint_Error;  -- tbd
      return 0.0; --blas.ASUM (Self);
   end;




   function Index_of_max (Self : in     Vector) return Integer
   is
   begin
      raise constraint_error;
      return 0; --blas.AMAX (Self);
   end;




--     function to_Degrees (Self : in Vector) return Vector
--     is
--     begin
--        return Self * (180.0 / Pi);
--     end;



   procedure swap (X, Y : in out Vector)
   is
   begin
      raise constraint_Error;   -- tbd:
      --blas.SWAP (X, Y);
   end;



   procedure copy (From : in     Vector;
                   To   : in out Vector)
   is
   begin
      raise constraint_error;  -- tbd:
      --blas.COPY (From, To);
   end;




   procedure scale (Self : in out Vector;   By : Real)
   is
   begin
      for Each in Self'range loop
         Self (Each) := Self (Each) * By;
      end loop;

      --blas.SCAL (By, Self);   -- tbd:
   end;



--     function "*" (Left : Vector;     Right : Number) return Vector
--     is
--        Result : Vector := Left;
--     begin
--        scale (Result, by => Right);
--        return Result;
--     end;
--
--
--     function "*" (Left : Number;     Right : Vector) return Vector
--     is
--     begin
--        return Right * Left;
--     end;


--     function "*" (Left : Vector;     Right : Vector) return Number
--     is
--     begin
--        return blas.DOT (Left, Right);
--     end;



   function "/" (Left : Vector;   Right : Real) return Vector
   is
      the_Result : Vector := Left;
   begin
      scale (the_Result, by => 1.0 / Right);
      return the_Result;
   end;




--     function "+" (Left : Vector;       Right : Vector)   return Vector
--     is
--        Result : Vector (Left'Range);
--     begin
--        pragma assert (Left'Length = Right'Length);
--
--        for Each in Result'Range loop
--           Result (Each) := Left (Each) + Right (Each);
--        end loop;
--
--        return Result;
--     end;



   function Min (Left : in Vector;   Right : in Vector) return Vector
   is
      the_Min : Vector (Left'Range);
   begin
      pragma assert (Left'Length = Right'Length);

      for Each in the_Min'range loop
         the_Min (Each) := Real'Min (Left (Each),  Right (Each));
      end loop;

      return the_Min;
   end;



   function Max (Left : in Vector;   Right : in Vector) return Vector
   is
      the_Max : Vector (Left'Range);
   begin
      pragma assert (Left'Length = Right'Length);

      for Each in the_Max'range loop
         the_Max (Each) := Real'Max (Left (Each),  Right (Each));
      end loop;

      return the_Max;
   end;



   function Image (Self : in Vector) return String
   is
      the_Image : String (1 .. 1 * 1024 * 1024);   -- handles one megabyte string, excess is truncated
      Count     : standard.Natural := 0;

      procedure add (Text : in String)
      is
      begin
         the_Image (Count + 1 .. Count + text'Length) := Text;
         Count                                        := Count + text'Length;
      end;

   begin
      add ("(");

      for Each in self'Range loop
         if Each /= self'First then
            add (", ");
         end if;

         add (Real'Image (Self (Each)));
      end loop;

      add (")");


      return the_Image (1 .. Count);

   exception
      when others =>
         return the_Image (1 .. Count);       -- tbd: refine this ...
   end;





   ------------------------------------------------------------------------------------------------------------------------------
   -- matrices
   --

   function to_Matrix (Row_1, Row_2, Row_3 : in Vector_3) return Matrix_3x3
   is
   begin
      return ((Row_1 (1), Row_1 (2), Row_1 (3)),
              (Row_2 (1), Row_2 (2), Row_2 (3)),
              (Row_3 (1), Row_3 (2), Row_3 (3)));

   end;





   function Min (Self : in Matrix) return Real
   is
      the_Min : Real := Real'Last;
   begin
      for each_Row in Self'Range (1) loop
         for each_Col in Self'Range (2) loop
            the_Min := Real'Min (the_Min,  Self (each_Row, each_Col));
         end loop;
      end loop;

      return the_Min;
   end;




   function Max (Self : in Matrix) return Real
   is
      the_Max : Real := Real'First;
   begin
      for each_Row in Self'Range (1) loop
         for each_Col in Self'Range (2) loop
            the_Max := Real'Max (the_Max,  Self (each_Row, each_Col));
         end loop;
      end loop;

      return the_Max;
   end;




   function Image (Self : in Matrix) return String
   is
      the_Image : String (1 .. 1 * 1024 * 1024);   -- handles one megabyte string, excess is truncated
      Count     : standard.Natural := 0;

      procedure add (Text : in String)
      is
      begin
         the_Image (Count + 1 .. Count + text'Length) := Text;
         Count                                        := Count + text'Length;
      end;

   begin
      add ("(");

      for Row in self'Range (1) loop
         add ((1 => ada.Characters.latin_1.LF));

         if Row /= self'First (1) then
            add (", ");
         end if;

         for Col in self'Range (2) loop
            if Col /= self'First (2) then
               add (", ");
            end if;

            add (Real'Image (Self (Row, Col)));
         end loop;

      end loop;

      add (")");


      return the_Image (1 .. Count);

   exception
      when others =>
         return the_Image (1 .. Count);       -- tbd: refine this ...
   end;





--     -- pragmarc
--     --
--
--     package pragmarc_Matrices is new pragmarc.Matrix_math (Number, -1.0, 0.0);
--
--
--     subtype pragmarc_Vector   is pragmarc_Matrices.Vector;
--     subtype pragmarc_Vector_3 is pragmarc_Matrices.Vector (3);
--     subtype pragmarc_Vector_4 is pragmarc_Matrices.Vector (4);
--
--     subtype pragmarc_Matrix   is pragmarc_Matrices.Matrix;
--     subtype pragmarc_Matrix_3 is pragmarc_Matrices.Matrix (3, 3);
--
--     use pragmarc_Matrices;
--     use type pragmarc_Matrix;
--     use type pragmarc_Matrix_3;
--
--
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
--
--
--
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
--
--
--
--
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
--
--
--
--
--
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








   function is_Square (Self : in Matrix) return Boolean
   is
   begin
      return self'Length (1) = self'Length (2);
   end;





--     function sub_Matrix (Self : in Matrix;   start_Row, end_Row : in Index_type;
--                                              start_Col, end_Col : in Index_type) return Matrix
   function sub_Matrix (Self : in Matrix;   start_Row, end_Row : in Integer;
                                            start_Col, end_Col : in Integer) return Matrix
   is
      the_sub_Matrix : Matrix (1 .. end_Row - start_Row + 1,
                               1 .. end_Col - start_Col + 1);
   begin
      for each_Row in the_sub_Matrix'Range (1) loop
         for each_Col in the_sub_Matrix'Range (2) loop
            the_sub_Matrix (each_Row, each_Col) := Self (each_Row + start_Row - 1,
                                                         each_Col + start_Col - 1);
         end loop;
      end loop;

      return the_sub_Matrix;
   end;





   --function Identity (Length : in Index_type := 3) return Matrix
   function Identity (Length : in Integer := 3) return Matrix
   is
      Result : Matrix (1 .. Length, 1 .. Length);
   begin
      for Row in 1 .. Length loop
         for Col in 1 .. Length loop
            if Row = Col then
               Result (Row, Col) := 1.0;
            else
               Result (Row, Col) := 0.0;
            end if;
         end loop;
      end loop;

      return Result;
   end;













   function "*" (Left  : in Matrix;   Right : in Real) return Matrix
   is
      Result : Matrix := Left;
   begin
      for Row in Result'Range (1) loop
         for Col in Result'Range (2) loop
            Result (Row, Col) := Result (Row, Col) * Right;
         end loop;
      end loop;

      return Result;
   end;





--     function "*" (Left : Matrix;   Right : Vector) return Vector
--     is
--        right_Data : math.Numbers                                           := right.Data;
--        Result     : math.Vector.standard.item (length => right_Data'Length);
--     begin
--        result.Data := (others => 0.0);
--
--        for Each in 1 .. result.Length loop
--           for each_Col in 1 .. left.num_Cols loop
--              if each_Col <= Right_Data'last then
--                 result.Data (Each) :=   result.Data (Each)
--                                       + left.Data (Each, each_Col) * Right_Data (each_Col);
--              end if;
--           end loop;
--        end loop;
--
--        return Result;
--  --        return (left (1, 1) * right (1)  +  left (1, 2) * right (2)  +  left (1, 3) * right (3),
--  --                left (2, 1) * right (1)  +  left (2, 2) * right (2)  +  left (2, 3) * right (3),
--  --                left (3, 1) * right (1)  +  left (3, 2) * right (2)  +  left (3, 3) * right (3));
--     end;



   package Real_arrays is new Ada.Numerics.Generic_Real_Arrays (Real);




--     function Transposed (Self : in Matrix) return Matrix
--     is
--     begin
--       return to_Math (transpose (to_pragmarc (Self)));
--     end;




--     procedure invert (Self : in out Matrix)
--     is
--     begin
--        Self := to_Math (invert (to_pragmarc (Self)));
--     end invert;





   procedure factor_Cholesky (Self : in out Matrix)
   is
   begin
      pragma assert (is_Square (Self));

      null;
   end;





--     procedure invert_positive_definite (Self : in out Matrix)
--     is
--        Status : math.Integer;
--     begin
--        pragma assert (is_Square (Self));
--
--        laCmp.DPOTRI (UPLO => 'U',                -- upper triangle option
--                      N    => Self'Length (1),
--                      A    => Self,
--                      LDA	 => Self'Length (1),    -- leading dimension of 'Self'
--                      INFO => Status);
--
--        if    Status < 0 then
--           raise constraint_Error with   "the" & integer'Image (Status) & "-th argument had an illegal value";
--        elsif Status > 0 then
--           raise constraint_Error with   "the (" & integer'Image (Status) & "," & integer'Image (Status)
--                                       & ") element of the factor U  or  L  is zero, and the inverse could not be computed";
--        end if;
--     end;





--     function  Inverted_positive_definite (Self : in Matrix) return Matrix
--     is
--        Result : Matrix := Self;
--     begin
--        invert_positive_definite (Result);
--        return Result;
--     end;





   ------------------------------------------------------------------------------------------------------------------------------
   -- Quaternions
   --


   function to_Quaternion (aX, aY, aZ : in     Real;
                           Angle      : in     Real) return Quaternion
   is
      Result : Quaternion;
      L      : Real      := aX * aX  +  aY * aY  +  aZ * aZ;
   begin
      if L > 0.0 then
         declare
            half_Angle : Real := Angle * 0.5;
         begin
            Result (1) := Cos (half_Angle);
            L          := Sin (half_Angle) * (1.0 / Sqrt (L));
            Result (2) := aX * L;
            Result (3) := aY * L;
            Result (4) := aZ * L;
         end;
      else
         Result (1) := L;
         Result (2) := 0.0;
         Result (3) := 0.0;
         Result (4) := 0.0;
      end if;

      return Result;
   end;





--     function "/" (Self : in    Quaternion;
--                   By   : in    Number)    return Quaternion
--     is
--        inverse_By : Number := 1.0 / By;
--     begin
--        return (Self (1) * inverse_By,
--                Self (2) * inverse_By,
--                Self (3) * inverse_By,
--                Self (4) * inverse_By);
--     end;





   function "*" (Self : in     Quaternion;
                 By   : in     Quaternion) return Quaternion
   is
      t    : constant := 1;
      x    : constant := 2;
      y    : constant := 3;
      z    : constant := 4;

      A    : Quaternion renames Self;
      B    : Quaternion renames By;

      AtBt : Real := A (t) * B (t);
      AxBx : Real := A (x) * B (x);
      AyBy : Real := A (y) * B (y);
      AzBz : Real := A (z) * B (z);

      AtBx : Real := A (t) * B (x);
      AxBt : Real := A (x) * B (t);
      AyBz : Real := A (y) * B (z);
      AzBy : Real := A (z) * B (y);

      AtBy : Real := A (t) * B (y);
      AxBz : Real := A (x) * B (z);
      AyBt : Real := A (y) * B (t);
      AzBx : Real := A (z) * B (x);

      AtBz : Real := A (t) * B (z);
      AxBy : Real := A (x) * B (y);
      AyBx : Real := A (y) * B (x);
      AzBt : Real := A (z) * B (t);
   begin
      return (AtBt - AxBx - AyBy - AzBz,
              AtBx + AxBt + AyBz - AzBy,
              AtBy - AxBz + AyBt + AzBx,
              AtBz + AxBy - AyBx + AzBt);
   end;




--     function "*" (Self : in     Quaternion;
--                   By   : in     Quaternion) return Quaternion
--     is
--        new_Vector : Vector_3 :=   Vector_3 (Self (2 .. 4))  *  By (1)
--                                 + Self (1)                  *  Vector_3 (By (2 .. 4))
--                                 + Vector_3 (Self (2 .. 4))  *  Vector_3 (By (2 .. 4));
--     begin
--        return (Self (1) * By (1)  -  Vector_3 (Self (2 .. 4)) * Vector_3 (By (2 .. 4)),
--                new_Vector (1),
--                new_Vector (2),
--                new_Vector (3));
--     end;

--    /// Multiply two quaternions, Grassmann product
--    inline friend csQuaternion operator* (const csQuaternion& q1,
--      const csQuaternion& q2)
--    {
--      return csQuaternion (q1.v * q2.w  +  q1.w * q2.v  +  q1.v % q2.v,
--        q1.w * q2.w  -  q1.v * q2.v);
--    }





--     function Dot (Self  : in Quaternion;
--                   Other : in Quaternion) return Number
--     is
--     begin
--        return Vector_3 (Self (2 .. 4)) * Vector_3 (Other (2 .. 4))  +  Self (1) * Other (1);
--     end;





--     function squared_Norm (Self : in Quaternion) return Number
--     is
--     begin
--        return Dot (Self, Self);
--     end;





--     function Norm (Self : in Quaternion) return Number
--     is
--     begin
--        return Sqrt (squared_Norm (Self));
--     end;





   function Unit (Self : in Quaternion) return Quaternion
   is
   begin
      return Quaternion (  Vector (Self)
                         / Norm (Vector (Self)));
   end;





   function infinitesimal_Rotation_from (Self             : in     Quaternion;
                                         angular_Velocity : in     Vector_3) return Quaternion
   is
      i_Rotation : Quaternion;
   begin
      i_Rotation (1) := 0.5 * (- angular_Velocity (1) * Self (2)
                               - angular_Velocity (2) * Self (3)
                               - angular_Velocity (3) * Self (4));

      i_Rotation (2) := 0.5 * (  angular_Velocity (1) * Self (1)
                               + angular_Velocity (2) * Self (4)
                               - angular_Velocity (3) * Self (3));

      i_Rotation (3) := 0.5 * (- angular_Velocity (1) * Self (4)
                               + angular_Velocity (2) * Self (1)
                               + angular_Velocity (3) * Self (2));

      i_Rotation (4) := 0.5 * (  angular_Velocity (1) * Self (3)
                               - angular_Velocity (2) * Self (2)
                               + angular_Velocity (3) * Self (1));
      return i_Rotation;
   end;





   function euler_Angles (Self : in     Quaternion) return Vector_3     -- 'self' can be non-normalised quaternion
   is
      w : Real renames Self (1);
      x : Real renames Self (2);
      y : Real renames Self (3);
      z : Real renames Self (4);

      the_Angles : Vector_3;
      bank       : Real renames the_Angles (1);
      heading    : Real renames the_Angles (2);
      attitude   : Real renames the_Angles (3);

      sqw : Real := w * w;
      sqx : Real := x * x;
      sqy : Real := y * y;
      sqz : Real := z * z;

      unit : Real := sqx + sqy + sqz + sqw;      -- if normalised is one, otherwise is correction factor
      test : Real := x * y  +  z * w;
   begin
      if test > 0.499 * unit then   -- singularity at north pole
         heading  := 2.0 * arcTan (x, w);
         attitude := Pi / 2.0;
         bank     := 0.0;
         return the_Angles;
      end if;

      if test < -0.499 * unit then   -- singularity at south pole
         heading  := -2.0 * arcTan (x, w);
         attitude := -Pi / 2.0;
         bank     := 0.0;
         return the_Angles;
      end if;

      heading  := arcTan (2.0 * y * w  -  2.0 * x * z,    sqx - sqy - sqz + sqw);
      attitude := arcSin (2.0 * test / unit);
      bank     := arcTan (2.0 * x * w  -  2.0 * y * z,   -sqx + sqy - sqz + sqw);
      return the_Angles;
   end;





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





   function conjugate (Self : in Quaternion) return Quaternion
   is
   begin
      return (Self (1),  -Self (2),  -Self (3),  -Self (4));
   end;




   procedure normalise (Self : in out Quaternion)
   is
   begin
      normalise (Vector (Self));
   end;



   function Normalised (Self : in     Quaternion) return Quaternion
   is
   begin
      return Quaternion (Normalised (Vector (Self)));
   end;



end math.Algebra.linear;


-- notes:
--
