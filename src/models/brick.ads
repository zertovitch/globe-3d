------------------------------------------------------------------------------
--  File:            Brick.ads
--  Description:     3D model of a "brick" of a space station
--                   Copyright (c) Gautier de Montmollin 2005
------------------------------------------------------------------------------

with GLOBE_3D;

package Brick is

  type Brick_Kind is (cube, cross);

  type Cubic_Face_Count is new Integer range 1 .. 6;

  type Cubic_Face_Set is array (Cubic_Face_Count) of Boolean;

  type Cubic_Face_Index is array (Cubic_Face_Count) of Positive;

  type Cubic_Face_Texture is array (Cubic_Face_Count) of GLOBE_3D.Image_ID;

  procedure Create
    (object  : in out GLOBE_3D.p_Object_3D;
     scale   :        GLOBE_3D.Real;     --  = unit (e.g, cube's side's length)
     centre  :        GLOBE_3D.Point_3D;
     kind    :        Brick_Kind;
     opening :        Cubic_Face_Set;    --  needed opening on which face
     portal  :    out Cubic_Face_Index;  --  corresponding connecting faces
     texture :        Cubic_Face_Texture);

end Brick;
