------------------------------------------------------------------------------
--  File:            Vehic002.ads
--  Description:     3D model of a space vehicle. Big, mysterious ship.
--                   Copyright (c) Gautier de Montmollin 1999-2000
------------------------------------------------------------------------------

with GLOBE_3D;

package Vehic002 is

  procedure Create(
    object: in out GLOBE_3D.p_Object_3D;
    scale :        GLOBE_3D.Real;
    centre:        GLOBE_3D.Point_3D;
    metal_door,
    metal_surface,
    bumped_blue  : GLOBE_3D.Image_id
  );

end Vehic002;
