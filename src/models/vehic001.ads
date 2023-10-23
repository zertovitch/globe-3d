------------------------------------------------------------------------------
--  File:            Vehic001.ads
--  Description:     3D model of a space vehicle. Small ship with wings.
--                   Copyright (c) Gautier de Montmollin 1999-2000
------------------------------------------------------------------------------

with GLOBE_3D;

package Vehic001 is

  procedure Create
    (object : in out GLOBE_3D.p_Object_3D;
     scale  :        GLOBE_3D.Real;
     centre :        GLOBE_3D.Point_3D);

end Vehic001;
