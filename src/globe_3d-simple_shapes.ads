package GLOBE_3D.Simple_Shapes is

  function Cylinder
    (disk_center_1 : Point_3D;  --  Center of "bottom" disc
     disk_center_2 : Point_3D;  --  Center of "top" disc
     radius        : Real;
     slices        : Positive;
     stacks        : Positive;
     colour        : GL.RGB_Color)
     return Object_3D;

  function Truncated_Cone
    (disk_center_1 : Point_3D;  --  Center of "bottom" disc
     disk_center_2 : Point_3D;  --  Center of "top" disc
     radius_1      : Real;
     radius_2      : Real;
     slices        : Positive;
     stacks        : Positive;
     colour        : GL.RGB_Color)
     return Object_3D;

end GLOBE_3D.Simple_Shapes;
