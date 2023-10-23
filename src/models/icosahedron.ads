with GLOBE_3D;

package Icosahedron is

  procedure Create
    (object   : in out GLOBE_3D.p_Object_3D;
     scale    :        GLOBE_3D.Real;
     centre   :        GLOBE_3D.Point_3D;
     alpha    :        GLOBE_3D.Real;
     polyball :        Boolean);

end Icosahedron;
