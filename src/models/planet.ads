with GLOBE_3D;

package Planet is

  procedure Create(
    object  : in out GLOBE_3D.p_Object_3D;
    scale   :        GLOBE_3D.Real;
    centre  :        GLOBE_3D.Point_3D;
    mercator:        GLOBE_3D.Image_ID;
    parts   :        Positive:= 30
  );

end Planet;
