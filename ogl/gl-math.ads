
with Ada.Numerics.Generic_Elementary_functions;
with Ada.Text_IO;



package GL.Math is


  package REF is new Ada.Numerics.Generic_Elementary_functions (Double);
  package RIO is new Ada.Text_IO.Float_IO                      (Double);



  -------------
  -- Vectors --
  -------------

  function "*"(l: Double; v: Double_Vector_3D) return Double_Vector_3D;
  pragma Inline("*");

  function "*"(v: Double_Vector_3D; l: Double) return Double_Vector_3D;
  pragma Inline("*");

  function "+"(a,b: Double_Vector_3D) return Double_Vector_3D;
  pragma Inline("+");

  function "-"(a: Double_Vector_3D) return Double_Vector_3D;
  pragma Inline("-");

  function "-"(a,b: Double_Vector_3D) return Double_Vector_3D;
  pragma Inline("-");

  function "*"(a,b: Double_Vector_3D) return Double;      -- dot product
  pragma Inline("*");

  function "*"(a,b: Double_Vector_3D) return Double_Vector_3D; -- cross product
  pragma Inline("*");

  function Norm(a: Double_Vector_3D) return Double;
  pragma Inline(Norm);

  function Norm2(a: Double_Vector_3D) return Double;
  pragma Inline(Norm2);

  function Normalized(a: Double_Vector_3D) return Double_Vector_3D;


  type Vector_4D is array (0 .. 3) of Double;



   -- Angles
   --

   function Angle (Point_1, Point_2, Point_3 : Double_Vector_3D) return Double;
   --
   -- returns the angle between the vector Point_1 to Point_2 and the vector Point_3 to Point_2.


   function to_Degrees (Radians : Double) return Double;
   function to_Radians (Degrees : Double) return Double;


  --------------
  -- Matrices --
  --------------

  type Matrix    is array (Positive range <>, Positive range <>) of aliased Double;
  type Matrix_33 is new Matrix (1..3, 1..3);
  type Matrix_44 is new Matrix (1..4, 1..4);
  --type Matrix_44 is array(0..3,0..3) of aliased Double; -- for GL.MultMatrix
  pragma Convention (Fortran, Matrix_44);                 -- GL stores matrices columnwise   -- tbd: use same convention for other matrices ?

  Id_33 : constant Matrix_33:= ((1.0, 0.0, 0.0),
                                (0.0, 1.0, 0.0),
                                (0.0, 0.0, 1.0));



  function "*"(A,B: Matrix_33) return Matrix_33;

  function "*"(A: Matrix_33; x: Double_Vector_3D) return Double_Vector_3D;
  function "*"(A: Matrix_44; x: Double_Vector_3D) return Double_Vector_3D;
  function "*"(A: Matrix_44; x: Double_Vector_3D) return Vector_4D;

  function Transpose(A: Matrix_33) return Matrix_33;
  function Transpose(A: Matrix_44) return Matrix_44;

  function Det(A: Matrix_33) return Double;

  function XYZ_rotation(ax,ay,az: Double) return Matrix_33;

  function XYZ_rotation(v: Double_Vector_3D) return Matrix_33;

  -- Gives a rotation matrix that corresponds to look into a certain
  -- direction. Camera swing rotation is arbitrary.
  -- Left-multiply by XYZ_Rotation(0.0,0.0,az) to correct it.
  function Look_at(direction: Double_Vector_3D) return Matrix_33;

   function Look_at (eye, center, up : Double_Vector_3D) return Matrix_33;


  -- This is for correcting cumulation of small computational
  -- errors, making the rotation matrix no more orthogonal
  procedure Re_Orthonormalize(M: in out Matrix_33);

  -- Right-multiply current matrix by A
  procedure Multiply_GL_Matrix( A: Matrix_33 );

  -- Impose A as current matrix
  procedure Set_GL_Matrix( A: Matrix_33 );

  -- For replacing the " = 0.0" test which is a Bad Thing
  function Almost_zero(x: Double) return Boolean;
  pragma Inline(Almost_zero);
  function Almost_zero(x: GL.Float) return Boolean;
  pragma Inline(Almost_zero);


  function sub_Matrix (Self : in Matrix;   start_Row, end_Row : in Positive;
                                           start_Col, end_Col : in Positive) return Matrix;


end GL.Math;
