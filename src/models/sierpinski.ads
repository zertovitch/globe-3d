with GLOBE_3D;

package Sierpinski is

  type Cubic_Face_Count is new Integer range 1 .. 6;
  type Cubic_Face_Texture is array (Cubic_Face_Count) of GLOBE_3D.Image_ID;

  procedure Create_Cube
    (object        : in out GLOBE_3D.p_Object_3D;
     scale         :        GLOBE_3D.Real;
     centre        :        GLOBE_3D.Point_3D;
     texture       :        Cubic_Face_Texture;
     tiled         :        Boolean;
     fractal_level :        Natural);

end Sierpinski;
