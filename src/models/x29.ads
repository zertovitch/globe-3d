------------------------------------------------------------------------------
--  File:            X29.adb
--  Description:     x29 aircraft data base from Evans & Sutherland
--  Copyright: (c) Evans & Sutherland -- ok to distribute if copyright appears
------------------------------------------------------------------------------

with GLOBE_3D;

package X29 is

  procedure Create
    (object : in out GLOBE_3D.p_Object_3D;
     scale  :        GLOBE_3D.Real;
     centre :        GLOBE_3D.Point_3D);

end X29;
