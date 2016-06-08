--  Helpers for portal rendering.
--  Methods for clipping through screen rectangles.

package GLOBE_3D.Portals is

  --  Intersect two rectangles into a third one.

  procedure Intersect (A,B: Rectangle; C: out Rectangle; non_empty: out Boolean);

  --  Find the smallest rectangle on screen in which the object will be displayed.

  procedure Find_bounding_box(
    o      :     Object_3D'Class;
    face   :     Positive;
    b      : out Rectangle;
    success: out Boolean
  );

  --  This is for experimentation or debugging: we show current bounding box (clip) for clipping.

  procedure Draw_boundary( main, clip: Rectangle; portal_depth: Natural:= 0 );

end GLOBE_3D.Portals;
