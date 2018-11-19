-------------------------------------------------------------------------
--  GL.Math
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

package body GL.Math is

  use REF;

  -------------
  -- Vectors --
  -------------

  function "*"(l: Double; v: Double_Vector_3D) return Double_Vector_3D is
  begin
    return (l*v(0),l*v(1),l*v(2));
  end "*";

  function "*"(v: Double_Vector_3D; l: Double) return Double_Vector_3D is
  begin
    return (l*v(0),l*v(1),l*v(2));
  end "*";

  function "+"(a,b: Double_Vector_3D) return Double_Vector_3D is
  begin
    return (a(0)+b(0),a(1)+b(1),a(2)+b(2));
  end "+";

  function "-"(a: Double_Vector_3D) return Double_Vector_3D is
  begin
    return (-a(0),-a(1),-a(2));
  end "-";

  function "-"(a,b: Double_Vector_3D) return Double_Vector_3D is
  begin
    return (a(0)-b(0),a(1)-b(1),a(2)-b(2));
  end "-";

  function "*"(a,b: Double_Vector_3D) return Double is             -- Dot product.
  begin
    return a(0)*b(0)+a(1)*b(1)+a(2)*b(2);
  end "*";

  function "*"(a,b: Double_Vector_3D) return Double_Vector_3D is   -- Cross product.
  begin
    return ( a(1)*b(2) - a(2)*b(1),
             a(2)*b(0) - a(0)*b(2),
             a(0)*b(1) - a(1)*b(0) );
  end "*";

  function Norm(a: Double_Vector_3D) return Double is
  begin
    return Sqrt(a(0)*a(0)+a(1)*a(1)+a(2)*a(2));
  end Norm;

  function Norm2(a: Double_Vector_3D) return Double is
  begin
    return a(0)*a(0)+a(1)*a(1)+a(2)*a(2);
  end Norm2;

  function Normalized(a: Double_Vector_3D) return Double_Vector_3D is
  begin
    return a * (1.0 / Norm(a));
  end Normalized;

  function Identical(a, b: Double_Vector_3D) return Boolean is
  begin
    return
      Almost_zero(a(0)-b(0)) and then
      Almost_zero(a(1)-b(1)) and then
      Almost_zero(a(2)-b(2));
  end Identical;

  function Identical(a, b: RGB_Color) return Boolean is
  begin
    return
      Almost_zero(a.red   - b.red)   and then
      Almost_zero(a.green - b.green) and then
      Almost_zero(a.blue  - b.blue);
  end Identical;

  function Identical(a, b: RGBA_Color) return Boolean is
  begin
    return
      Almost_zero(a.red   - b.red)   and then
      Almost_zero(a.green - b.green) and then
      Almost_zero(a.blue  - b.blue)  and then
      Almost_zero(a.alpha - b.alpha);
  end Identical;

   -- Angles
   --

   function Angle (Point_1, Point_2, Point_3 : Double_Vector_3D) return Double
   is
      Vector_1  : constant Double_Vector_3D := Normalized (Point_1 - Point_2);
      Vector_2  : constant Double_Vector_3D := Normalized (Point_3 - Point_2);
      Cos_Theta : constant Double           := Vector_1 * Vector_2;
   begin
      if Cos_Theta <= -1.0 then
         return Ada.Numerics.Pi;

      elsif Cos_Theta >= 1.0 then
         return 0.0;

      else
         return Arccos (Cos_Theta);
      end if;
   end Angle;

   function to_Degrees (Radians : Double) return Double
   is
      use Ada.Numerics;
   begin
      return Radians * 180.0 / Pi;
   end to_Degrees;

   function to_Radians (Degrees : Double) return Double
   is
      use Ada.Numerics;
   begin
      return Degrees * Pi / 180.0;
   end to_Radians;

  --------------
  -- Matrices --
  --------------

  function "*"(A,B: Matrix_33) return Matrix_33 is
    r: Double; AB: Matrix_33;
  begin
    for i in 1..3 loop
      for j in 1..3 loop
        r:= 0.0;
        for k in 1..3 loop
          r:= r + (A(i,k) * B(k,j));
        end loop;
        AB(i,j):= r;
      end loop;
    end loop;
    return AB;
  end "*";

  function "*"(A: Matrix_33; x: Double_Vector_3D) return Double_Vector_3D is
    r: Double;
    Ax: Double_Vector_3D;
    -- banana skin: Matrix has range 1..3, Vector 0..2 (GL)
  begin
    for i in 1..3 loop
      r:= 0.0;
      for j in 1..3 loop
          r:= r + A(i,j) * x(j-1);
      end loop;
      Ax(i-1):= r;
    end loop;
    return Ax;
  end "*";

  function "*"(A: Matrix_44; x: Double_Vector_3D) return Double_Vector_3D is
    r: Double;

      type Vector_4D is array (0..3) of GL.Double;
      x_4D : constant Vector_4D := (x(0), x(1), x(2) , 1.0);
      Ax   : Vector_4D;

    -- banana skin: Matrix has range 1..3, Vector 0..2 (GL)
  begin
    for i in 1..4 loop
      r:= 0.0;
      for j in 1..4 loop
          r:= r + A(i,j) * x_4D(j-1);
      end loop;
      Ax(i-1):= r;
    end loop;
    return (Ax (0), Ax (1), Ax(2));
  end "*";

  function "*"(A: Matrix_44; x: Double_Vector_3D) return Vector_4D is
    r: Double;

      x_4D : constant Vector_4D := (x(0), x(1), x(2) , 1.0);
      Ax   : Vector_4D;

    -- banana skin: Matrix has range 1..3, Vector 0..2 (GL)
  begin
    for i in 1..4 loop
      r:= 0.0;
      for j in 1..4 loop
          r:= r + A(i,j) * x_4D(j-1);
      end loop;
      Ax(i-1):= r;
    end loop;
    return Ax;
  end "*";

  function Transpose(A: Matrix_33) return Matrix_33 is
  begin
    return ( (A(1,1),A(2,1),A(3,1)),
             (A(1,2),A(2,2),A(3,2)),
             (A(1,3),A(2,3),A(3,3)));
  end Transpose;

  function Transpose(A: Matrix_44) return Matrix_44 is
  begin
    return ( (A(1,1),A(2,1),A(3,1),A(4,1)),
             (A(1,2),A(2,2),A(3,2),A(4,2)),
             (A(1,3),A(2,3),A(3,3),A(4,3)),
             (A(1,4),A(2,4),A(3,4),A(4,4)));
  end Transpose;

  function Det(A: Matrix_33) return Double is
  begin
    return
      A(1,1) * A(2,2) * A(3,3) +
      A(2,1) * A(3,2) * A(1,3) +
      A(3,1) * A(1,2) * A(2,3) -
      A(3,1) * A(2,2) * A(1,3) -
      A(2,1) * A(1,2) * A(3,3) -
      A(1,1) * A(3,2) * A(2,3);
  end Det;

  function XYZ_rotation(ax,ay,az: Double) return Matrix_33 is
    Mx, My, Mz: Matrix_33; c,s: Double;
  begin
    -- Around X.
    c:= Cos( ax );
    s:= Sin( ax );
    Mx:= ( (1.0,0.0,0.0),
           (0.0,  c, -s),
           (0.0,  s,  c) );
    -- Around Y.
    c:= Cos( ay );
    s:= Sin( ay );
    My:= ( (  c,0.0, -s),
           (0.0,1.0,0.0),
           (  s,0.0,  c) );
    -- Around Z.
    c:= Cos( az );
    s:= Sin( az );
    Mz:= ( (  c, -s,0.0),
           (  s,  c,0.0),
           (0.0,0.0,1.0) );

    return Mz * My * Mx;

  end XYZ_rotation;

  function XYZ_rotation(v: Double_Vector_3D) return Matrix_33 is
  begin
    return XYZ_rotation(v(0),v(1),v(2));
  end XYZ_rotation;

  function Look_at(direction: Double_Vector_3D) return Matrix_33 is
    v1, v2, v3: Double_Vector_3D;
  begin
    -- GL's look direction is the 3rd dimension (z).
    v3:= Normalized(direction);
    v2:= Normalized((v3(2),0.0,-v3(0)));
    v1:= v2 * v3;
    return
      (((v1(0),v2(0),v3(0)),
        (v1(1),v2(1),v3(1)),
        (v1(2),v2(2),v3(2))
      ));
  end Look_at;

   function sub_Matrix (Self : in Matrix;   start_Row, end_Row : in Positive;
                                            start_Col, end_Col : in Positive) return Matrix
   is
      the_sub_Matrix : Matrix (1 .. end_Row - start_Row + 1,
                               1 .. end_Col - start_Col + 1);
   begin
      for Row in the_sub_Matrix'Range (1) loop
         for Col in the_sub_Matrix'Range (2) loop
            the_sub_Matrix (Row, Col) := Self (Row + start_Row - 1,
                                               Col + start_Col - 1);
         end loop;
      end loop;

      return the_sub_Matrix;
   end sub_Matrix;

   function Look_at (eye, center, up : Double_Vector_3D) return Matrix_33
   is
      forward : constant Double_Vector_3D := Normalized ((center (0) - eye (0),  center (1) - eye (1),  center (2) - eye (2)));
      side    : constant Double_Vector_3D := Normalized (forward * up);
      new_up  : constant Double_Vector_3D := side * forward;
   begin
      return (( side    (0),    side    (1),    side    (2)),
              ( new_up  (0),    new_up  (1),    new_up  (2)),
              (-forward (0),   -forward (1),   -forward (2)));
   end Look_at;

  -- Following procedure is from Project Spandex, by Paul Nettle.
  --
  procedure Re_Orthonormalize(M: in out Matrix_33) is
    dot1,dot2,vlen: Double;
  begin
    dot1:= M(1,1) * M(2,1) + M(1,2) * M(2,2) + M(1,3) * M(2,3);
    dot2:= M(1,1) * M(3,1) + M(1,2) * M(3,2) + M(1,3) * M(3,3);

    M(1,1) := M(1,1) - dot1 * M(2,1) - dot2 * M(3,1);
    M(1,2) := M(1,2) - dot1 * M(2,2) - dot2 * M(3,2);
    M(1,3) := M(1,3) - dot1 * M(2,3) - dot2 * M(3,3);

    vlen:= 1.0 / Sqrt(M(1,1) * M(1,1) +
                      M(1,2) * M(1,2) +
                      M(1,3) * M(1,3));

    M(1,1):= M(1,1) * vlen;
    M(1,2):= M(1,2) * vlen;
    M(1,3):= M(1,3) * vlen;

    dot1:= M(2,1) * M(1,1) + M(2,2) * M(1,2) + M(2,3) * M(1,3);
    dot2:= M(2,1) * M(3,1) + M(2,2) * M(3,2) + M(2,3) * M(3,3);

    M(2,1) := M(2,1) - dot1 * M(1,1) - dot2 * M(3,1);
    M(2,2) := M(2,2) - dot1 * M(1,2) - dot2 * M(3,2);
    M(2,3) := M(2,3) - dot1 * M(1,3) - dot2 * M(3,3);

    vlen:= 1.0 / Sqrt(M(2,1) * M(2,1) +
                      M(2,2) * M(2,2) +
                      M(2,3) * M(2,3));

    M(2,1):= M(2,1) * vlen;
    M(2,2):= M(2,2) * vlen;
    M(2,3):= M(2,3) * vlen;

    M(3,1):= M(1,2) * M(2,3) - M(1,3) * M(2,2);
    M(3,2):= M(1,3) * M(2,1) - M(1,1) * M(2,3);
    M(3,3):= M(1,1) * M(2,2) - M(1,2) * M(2,1);
  end Re_Orthonormalize;

--    type Matrix_44 is array(0..3,0..3) of aliased Double; -- for GL.MultMatrix
--    pragma Convention(Fortran, Matrix_44);                -- GL stores matrices columnwise

  M: Matrix_44;
  -- M is a global variable for a clean 'Access and for setting once 4th dim.
  pM: constant GL.doublePtr:= M (1, 1)'Unchecked_Access;

  procedure Multiply_GL_Matrix( A: Matrix_33 ) is
  begin
    for i in 1..3 loop
      for j in 1..3 loop
        M(i,j):= A(i,j);
        --  Funny deformations...
        --  if j=2 then
        --    M(i-1,j-1):= 0.5 * A(i,j);
        --  end if;
      end loop;
    end loop;
    GL.MultMatrixd(pM);
  end Multiply_GL_Matrix;

  procedure Set_GL_Matrix( A: Matrix_33 ) is
  begin
    GL.LoadIdentity;
    Multiply_GL_Matrix( A );
  end Set_GL_Matrix;

  -- Ada 95 Quality and Style Guide, 7.2.7:
  -- Tests for
  --
  -- (1) absolute "equality" to 0 in storage,
  -- (2) absolute "equality" to 0 in computation,
  -- (3) relative "equality" to 0 in storage, and
  -- (4) relative "equality" to 0 in computation:
  --
  --  abs X <= Float_Type'Model_Small                      -- (1)
  --  abs X <= Float_Type'Base'Model_Small                 -- (2)
  --  abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
  --  abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)

  function Almost_zero(x: Double) return Boolean is
  begin
    return  abs x <= Double'Base'Model_Small;
  end Almost_zero;

  function Almost_zero(x: GL.Float) return Boolean is
  begin
    return  abs x <= GL.Float'Base'Model_Small;
  end Almost_zero;

begin
  for i in 1..3 loop
    M(i,4):= 0.0;
    M(4,i):= 0.0;
  end loop;
  M(4,4):= 1.0;
end GL.Math;
