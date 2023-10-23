--  This package allows to test basic shape for extruded surfaces

with GLOBE_3D;

package Extruded_Surface is

  type Kind_of_surface is (square, sphere);

  procedure Create
    (object     : in out GLOBE_3D.p_Object_3D;
     scale      :        GLOBE_3D.Real;
     centre     :        GLOBE_3D.Point_3D;
     grid       : in     Positive;
     surface    : in     Kind_of_surface;
     max_u3     : in     GLOBE_3D.Real;
     iterations : in     Natural;
     hor_tex,
     ver_tex    : in     GLOBE_3D.Image_ID;
     tiling_hu,
     tiling_hv,
     tiling_vu,
     tiling_vv  : in     Positive);

end Extruded_Surface;
