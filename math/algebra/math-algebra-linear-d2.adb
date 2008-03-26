
with math.fast_Trigonometry;
with math.fast_Rotation;

with ada.numerics.generic_real_arrays;

with ada.Strings.unbounded;    use ada.Strings.unbounded;
with ada.Characters.latin_1;   use ada.Characters.latin_1;




package body math.Algebra.linear.d2 is


   use math.Functions;
   use type math.Number, math.Integer;



   NL : constant String := (1 => ada.Characters.latin_1.LF);



   -- Vector_2
   --


   function "-" (Self : Vector_2) return Vector_2
   is
   begin
      return (-Self (1), -Self (2));
   end;





   function "-" (Left : Vector_2;     Right : Vector_2) return Vector_2
   is
   begin
      return (Left (1) - Right (1),
              Left (2) - Right (2));
   end;


   function "+" (Left : Vector_2;     Right : Vector_2) return Vector_2
   is
   begin
      return (Left (1) + Right (1),
              Left (2) + Right (2));
   end;




   function "*" (Left : Vector_2;     Right : Vector_2) return Number
   is
   begin
      return Left (1) * Right (1)  +  Left (2) * Right (2);
   end;




   function "*" (Left : Vector_2;     Right : Number) return Vector_2
   is
   begin
      return (Left (1) * Right,
              Left (2) * Right);
   end;



   function "*" (Left : Number;     Right : Vector_2) return Vector_2
   is
   begin
      return (Right (1) * Left,
              Right (2) * Left);
   end;





   function min (Left : Vector_2;     Right : Vector_2) return Vector_2
   is
   begin
      return (1 => number'Min (Left (1), Right (1)),
              2 => number'Min (Left (2), Right (2)));
   end;



   function max (Left : Vector_2;     Right : Vector_2) return Vector_2
   is
   begin
      return (1 => number'Max (Left (1), Right (1)),
              2 => number'Max (Left (2), Right (2)));
   end;




--     function Norm2 (Self : Vector_2) return math.Number
--     is
--     begin
--        return Self (1) * Self (1)  +  Self (2) * Self (2);
--     end;



   function Cross (Left  : in Vector_2;   Right : in Vector_2) return math.Number
   is
   begin
      return Left (1) * Right (2)  -  Left (2) * Right (1);
   end;





--     function Cross (Self : in Vector_2;   Scale : in Number) return Vector_2
--     is
--        Scaled : Vector_2 := Self;
--     begin
--        algebra.linear.scale (Vector (Scaled), Scale);
--
--        return (Scaled (2),  -Scaled (1));
--     end;
--
--
--
--     function Cross (Scale : in Number;   Self : in Vector_2) return Vector_2
--     is
--        Scaled : Vector_2 := Self;
--     begin
--        algebra.linear.scale (Vector (Scaled), Scale);
--
--        return (-Scaled (2),  Scaled (1));
--     end;




   function Cross (Self : in Vector_2;   Scale : in Number) return Vector_2
   is
   begin
      return (Scale * Self (2),  -Scale * Self (1));
   end;



   function Cross (Scale : in Number;   Self : in Vector_2) return Vector_2
   is
   begin
      return (-Scale * Self (2),  Scale * Self (1));
   end;



   function Normalise (Self : access Vector_2) return Number
   is
      Length : Number := Norm (Vector (Self.all));
   begin
      if Length < Number'Small then
         return 0.0;
      end if;

      declare
         inv_Length : Number := 1.0 / Length;
      begin
         Self (1) := Self (1) * inv_Length;
         Self (2) := Self (2) * inv_Length;
      end;

      return Length;
   end;




   function Norm   (Self : in Vector_2) return Number
   is
   begin
      return sqRt (Norm_2 (Self));
   end;




   function Norm_2 (Self : in Vector_2) return Number
   is
   begin
      return Self (1) * Self (1)  +  Self (2) * Self (2);
   end;




   procedure normalise (Self : in out Vector_2)
   is
     inv_Norm : constant Number := 1.0 / Norm (Self);
   begin
      self := (1 => Self (1) * inv_Norm,
               2 => Self (2) * inv_Norm);
   end;



   function Normalised (Self : in Vector_2) return Vector_2
   is
      Result : Vector_2 := Self;
   begin
      normalise (Result);
      return Result;
   end;



   function Centroid (Self : Vector_2_Array) return Vector_2
   is
      the_Centroid    : Vector_2 := (0.0, 0.0);
      reference_Point : Vector_2 := (0.0, 0.0);    -- the reference point for forming triangles.
                                                   -- it's location doesn't change the result (except for rounding error).
      Area            : Number   := 0.0;
      Inv_3           : constant := 1.0 / 3.0;
   begin
      pragma assert (self'Length >= 3);

      --  #if 0
      --  	// This code would put the reference point inside the polygon.
      --  	for (int32 i = 0; i < count; ++i)
      --  	{
      --  		pRef += vs[i];
      --  	}
      --  	pRef *= 1.0f / count;
      --  #endif

      for Each in Self'range loop
         declare
            p1 : Vector_2 := reference_Point;
            p2 : Vector_2 := Self (Each);

            function p3_Index return math.Integer is
            begin
               if Each + 1 <= Self'Last then   return Each + 1;   else   return Self'First;   end if;
            end;
            p3 : Vector_2 := Self (p3_Index);

            e1 : Vector_2 := p2 - p1;
            e2 : Vector_2 := p3 - p1;

            D  : Number   := cross (e1, e2);

            triangle_Area : Number := 0.5 * D;
         begin
            Area         := Area + triangle_Area;
            the_Centroid := the_Centroid  +  triangle_Area * Inv_3 * (p1 + p2 + p3);   -- area weighted centroid
         end;
      end loop;

      pragma assert (Area > number'Small);

      the_Centroid := the_Centroid * (1.0 / Area);
      return the_Centroid;
   end;




   function Clamped (Self : in Vector_2;   Low  : in Vector_2;
                                           High : in Vector_2) return Vector_2
   is
   begin
      return Max (Low,  Min (Self, High));
   end;


--     inline b2Vec2 b2Clamp(const b2Vec2& a, const b2Vec2& low, const b2Vec2& high)
--  {
--  	return b2Max(low, b2Min(a, high));
--  }



   -- Matrix_2x2
   --

   function to_Rotation (Angle : in Number) return access constant Matrix_2x2
   is
   begin
      return fast_rotation.to_Rotation (Angle);
   end;


--     function to_Rotation (Angle : in Number) return Matrix_2x2
--     is
--  --        C : constant Number := fast_trigonometry.cos (Angle);
--  --        S : constant Number := fast_trigonometry.sin (Angle);
--        C : Number;
--        S : Number;
--     begin
--        fast_trigonometry.get (Angle, the_cos => C,
--                                      the_sin => S);
--        return ((C, -S),
--                (S,  C));
--     end;




--     procedure set_Rotation (Self : access Matrix_2x2;   the_Angle : in Number)
--     is
--  --        C : constant Number := fast_trigonometry.cos (the_Angle);
--  --        S : constant Number := fast_trigonometry.sin (the_Angle);
--        C : Number;
--        S : Number;
--     begin
--        fast_trigonometry.get (the_Angle, the_cos => C,
--                                          the_sin => S);
--        Self.all := ((C, -S),
--                     (S,  C));
--     end;




--  	void Set(float32 angle)
--  	{
--  		float32 c = cosf(angle), s = sinf(angle);
--  		col1.x = c; col2.x = -s;
--  		col1.y = s; col2.y = c;
--  	}


--   function "*" (Left : Matrix_2x2;       Right : Vector_2)       return Vector_2
   function mul   (Left : access constant Matrix_2x2;   Right : access Vector_2) return Vector_2
   is
   begin
      return (Left (1, 1) * Right (1)  +  Left (1, 2) * Right (2),
              Left (2, 1) * Right (1)  +  Left (2, 2) * Right (2));
   end;
--     is
--        r  : Number;
--        Ax : Vector_2;
--     begin
--        for i in math.Integer'(1) .. 2 loop
--           r := 0.0;
--           for j in math.Integer'(1) .. 2 loop
--              r := r  +  Left (i, j)  *  Right (j);
--           end loop;
--           Ax (i) := r;
--        end loop;
--        return Ax;
--     end;




   function mul_T (L : access constant Matrix_2x2;   R : access Vector_2) return Vector_2
   is
   begin

      return (R.all  *  Vector_2'(L (1, 1),  L (2, 1)),
              R.all  *  (L (1, 2),  L (2, 2)));
   end;
--	u.Set(b2Dot(v, A.col1), b2Dot(v, A.col2));



   function mul   (Left : access constant Matrix_2x2;       Right : access Matrix_2x2)     return Matrix_2x2
   --function "*" (Left : Matrix_2x2;       Right : Matrix_2x2)     return Matrix_2x2
   is
      r  : Number;
      AB : Matrix_2x2;
   begin
      for i in math.Integer'(1) .. 2 loop
         for j in math.Integer'(1) .. 2 loop
            r := 0.0;
            for k in math.Integer'(1) .. 2 loop
               r := r + Left (i, k) * Right (k, j);
            end loop;
            AB (i, j) := r;
         end loop;
      end loop;
      return AB;
   end;




   function mul_T (Left : access constant Matrix_2x2;       Right : access Matrix_2x2)     return Matrix_2x2
   is
      c1 : constant Vector_2 := ((left (1, 1), left (2, 1))  *  (right (1, 1), right (2, 1)),
                                 (left (1, 2), left (2, 2))  *  (right (1, 1), right (2, 1)));

      c2 : constant Vector_2 := ((left (1, 1), left (2, 1))  *  (right (1, 2), right (2, 2)),
                                 (left (1, 2), left (2, 2))  *  (right (1, 2), right (2, 2)));

   begin
      return (1 => (c1 (1), c2 (1)),
              2 => (c1 (2), c2 (2)));
   end;
--  	b2Vec2 c1; c1.Set(b2Dot(A.col1, B.col1), b2Dot(A.col2, B.col1));
--  	b2Vec2 c2; c2.Set(b2Dot(A.col1, B.col2), b2Dot(A.col2, B.col2));
--  	b2Mat22 C;
--  	C.Set(c1, c2);
--  	return C;





--    function "*"(A,B: Matrix_33) return Matrix_33 is
--      r: Double; AB: Matrix_33;
--    begin
--      for i in 1..3 loop
--        for j in 1..3 loop
--          r:= 0.0;
--          for k in 1..3 loop
--            r:= r + (A(i,k) * B(k,j));
--          end loop;
--          AB(i,j):= r;
--        end loop;
--      end loop;
--      return AB;
--    end "*";



   function "abs" (Self : in Matrix_2x2) return Matrix_2x2
   is
   begin
      return (1 => (abs (Self (1, 1)),  abs (Self (1, 2))),
              2 => (abs (Self (2, 1)),  abs (Self (2, 2))));
   end;





   function Solve (Self : in Matrix_2x2;   B : in Vector_2) return Vector_2
   is
      A11 : Number renames Self (1, 1);
      A12 : Number renames Self (1, 2);
      A21 : Number renames Self (2, 1);
      A22 : Number renames Self (2, 2);

      det : Number := a11 * a22 - a12 * a21;
   begin
      pragma assert (det /= 0.0);
      det := 1.0 / det;

      return (1 => (det * (a22 * b (1) - a12 * b (2))),
              2 => (det * (a11 * b (2) - a21 * b (1))));
   end;

--  	b2Vec2 Solve(const b2Vec2& b) const
--  	{
--  		float32 a11 = col1.x, a12 = col2.x, a21 = col1.y, a22 = col2.y;
--  		float32 det = a11 * a22 - a12 * a21;
--  		b2Assert(det != 0.0f);
--  		det = 1.0f / det;
--  		b2Vec2 x;
--  		x.x = det * (a22 * b.x - a12 * b.y);
--  		x.y = det * (a11 * b.y - a21 * b.x);
--  		return x;
--  	}




   -- Transform
   --

   --function "*" (L : Transform_2d;   R : Vector_2) return Vector_2
   function mul (L : access constant Transform_2d;   R : access Vector_2) return Vector_2
   is
   begin
      --return L.Position + L.Rotation * R;
      return L.Position + mul (L.Rotation, R);
   end;






   function mul_T (L : access constant Transform_2d;   R : access Vector_2) return Vector_2
   is
      Right : aliased Vector_2 := R.all - l.Position;
   begin
      return Mul_T (l.Rotation,  Right'access);
   end;




   function Image (Self : in Transform_2d) return String
   is
      Pad : unbounded_String;
   begin
      append (Pad, NL & "Position: " & Image (Vector (self.Position)));
      append (Pad, NL & "Rotation: " & Image (Matrix (self.Rotation.all)));

      return to_String (Pad);
   end;


end math.Algebra.linear.d2;


-- notes:
--
