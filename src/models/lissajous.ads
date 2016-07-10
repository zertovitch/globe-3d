-- VRML: [#VRML V1.0 ascii
-- ]
with GLOBE_3D;

package Lissajous is

  procedure Create(
    object: in out GLOBE_3D.p_Object_3D;
    scale :        GLOBE_3D.Real;
    centre:        GLOBE_3D.Point_3D
  );

end Lissajous;
