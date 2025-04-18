------------------------------------------------------------------------------
--  File:            X29.adb
--  Description:     x29 aircraft data base from Evans & Sutherland
--  Copyright: (c) Evans & Sutherland -- ok to distribute if copyright appears
------------------------------------------------------------------------------

with GLOBE_3D.Sprite;

package X29_vbo is

  procedure Create
    (object : in out GLOBE_3D.Sprite.p_Sprite;
     scale  :        GLOBE_3D.Real;
     centre :        GLOBE_3D.Point_3D);

end X29_vbo;
