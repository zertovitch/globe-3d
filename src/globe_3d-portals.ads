package GLOBE_3D.Portals is

  -- Methods provided for clipping through screen rectangles

  procedure Intersect (A,B: Rectangle; C: out Rectangle; non_empty: out Boolean);

  procedure Find_bounding_box(
    o      :     Object_3D'Class;
    face   :     Positive;
    b      : out Rectangle;
    success: out Boolean
  );

  procedure Draw_boundary( main, clip: Rectangle );

end GLOBE_3D.Portals;

