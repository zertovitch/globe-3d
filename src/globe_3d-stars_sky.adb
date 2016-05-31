with GLOBE_3D.Math;
pragma Elaborate_All(GLOBE_3D.Math);

with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;

package body GLOBE_3D.Stars_sky is

  col: array(1..num_stars) of GL.RGB_Color;
  pnt: array(1..num_stars) of Point_3D; -- normalized position on sphere

  procedure Display(Rotation: Matrix_33) is
    use GL, GLOBE_3D.Math;
  begin
    PushMatrix;
    Set_GL_Matrix(Rotation);
    Disable( TEXTURE_2D );
    for i in 1..num_stars loop
      Color( col(i) );
      GL_Begin(GL.POINTS);
      Vertex(pnt(i));
      GL_End;
    end loop;
    PopMatrix;
  end Display;

  procedure Reset is
    seed: Generator;
    v: Vector_3D;
    int: Real;
    use GL, REF, GLOBE_3D.Math;

    function Amas return Real is -- expected tendencies: keep near or go far
      r: Real;
    begin
      r:= Real(Random(seed));
      r:= r * 2.0 - 1.0;       -- r in -1 .. 1
      r:= r ** 8;              -- almost always ~0
      r:= Exp(r*1.8)-1.0;
      return r;
    end Amas;

  begin
    Reset(seed);
    v:= (far_side,0.0,0.0);
    for i in 1..num_stars loop
      v:= XYZ_rotation( Amas, Amas, Amas ) * v;
      pnt(i):= v;
      int:= Real(Random(seed)) * 0.3;
      col(i):= (
        int+0.15*Real(Random(seed)),
        int+0.12*Real(Random(seed)),
        int+0.12*Real(Random(seed))
      );
    end loop;
  end Reset;

begin
  Reset;
end GLOBE_3D.Stars_sky;
