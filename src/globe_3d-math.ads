package GLOBE_3D.Math is

  -------------
  -- Vectors --
  -------------

  function "*"(l: Real; v: Vector_3D) return Vector_3D;
  pragma Inline("*");

  function "*"(v: Vector_3D; l: Real) return Vector_3D;
  pragma Inline("*");

  function "+"(a,b: Vector_3D) return Vector_3D;
  pragma Inline("+");

  function "-"(a: Vector_3D) return Vector_3D;
  pragma Inline("-");

  function "-"(a,b: Vector_3D) return Vector_3D;
  pragma Inline("-");

  function "*"(a,b: Vector_3D) return Real;      -- dot product
  pragma Inline("*");

  function "*"(a,b: Vector_3D) return Vector_3D; -- cross product
  pragma Inline("*");

  function Norm(a: Vector_3D) return Real;
  pragma Inline(Norm);

  function Norm2(a: Vector_3D) return Real;
  pragma Inline(Norm2);

  function Normalized(a: Vector_3D) return Vector_3D;

   -- Angles
   --

   function Angle (Point_1, Point_2, Point_3 : Vector_3D) return Real;
   --
   -- returns the angle between the vector Point_1 to Point_2 and the vector Point_3 to Point_2.

   function to_Degrees (Radians : Real) return Real;
   function to_Radians (Degrees : Real) return Real;

  --------------
  -- Matrices --
  --------------

  function "*"(A,B: Matrix_33) return Matrix_33;

  function "*"(A: Matrix_33; x: Vector_3D) return Vector_3D;
  function "*"(A: Matrix_44; x: Vector_3D) return Vector_3D;
  function "*"(A: Matrix_44; x: Vector_3D) return Vector_4D;

  function Transpose(A: Matrix_33) return Matrix_33;
  function Transpose(A: Matrix_44) return Matrix_44;

  function Det(A: Matrix_33) return Real;

  function XYZ_rotation(ax,ay,az: Real) return Matrix_33;

  function XYZ_rotation(v: Vector_3D) return Matrix_33;

  -- Gives a rotation matrix that corresponds to look into a certain
  -- direction. Camera swing rotation is arbitrary.
  -- Left-multiply by XYZ_Rotation(0.0,0.0,az) to correct it.
  function Look_at(direction: Vector_3D) return Matrix_33;

   function Look_at (eye, center, up : Vector_3D) return Matrix_33;

  -- This is for correcting cumulation of small computational
  -- errors, making the rotation matrix no more orthogonal
  procedure Re_Orthonormalize(M: in out Matrix_33);

  -- Right-multiply current matrix by A
  procedure Multiply_GL_Matrix( A: Matrix_33 );

  -- Impose A as current matrix
  procedure Set_GL_Matrix( A: Matrix_33 );

  -- For replacing the " = 0.0" test which is a Bad Thing
  function Almost_zero(x: Real) return Boolean;
  pragma Inline(Almost_zero);
  function Almost_zero(x: GL.Float) return Boolean;
  pragma Inline(Almost_zero);

  function sub_Matrix (Self : in Matrix;   start_Row, end_Row : in Positive;
                                           start_Col, end_Col : in Positive) return Matrix;

end GLOBE_3D.Math;
