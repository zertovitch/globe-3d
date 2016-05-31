------------------------------------------------------------------------------
--  File:            Brick.ads
--  Description:     3D model of a "brick" of a space station
--                   Copyright (c) Gautier de Montmollin 2005
------------------------------------------------------------------------------

with GLOBE_3D;

package Brick is

  type Brick_kind is (cube, cross);

  type Cubic_Face_count is new Integer range 1..6;

  type Cubic_Face_set is array(Cubic_Face_count) of Boolean;

  type Cubic_Face_index is array(Cubic_Face_count) of Positive;

  type Cubic_Face_texture is array(Cubic_Face_count) of GLOBE_3D.Image_ID;

  procedure Create(
    object : in out GLOBE_3D.p_Object_3D;
    scale  :        GLOBE_3D.Real;     -- = unit (e.g, cube's side's length)
    centre :        GLOBE_3D.Point_3D;
    kind   :        Brick_kind;
    opening:        Cubic_Face_set;    -- needed opening on which face
    portal :    out Cubic_Face_index;  -- corresponding connecting faces
    texture:        Cubic_Face_texture
  );

end Brick;
