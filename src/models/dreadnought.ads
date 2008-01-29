-- Title: Dreadnought class Heavy Cruiser
-- Subject: A space cruiser inspired by Sulaco from Aliens.
-- Author: Xenodroid

with GLOBE_3D;

package Dreadnought is

  procedure Create(
    object: in out GLOBE_3D.p_Object_3D;
    scale :        GLOBE_3D.Real;
    centre:        GLOBE_3D.Point_3D;
    alum_001,
    grumnoir,
    tole_001,
    alum_002: GLOBE_3D.Image_ID
  );
end;
