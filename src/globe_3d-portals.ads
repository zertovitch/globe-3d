------------------------------------
--  Helpers for portal rendering  --
------------------------------------

package GLOBE_3D.Portals is

  --  Intersect two rectangles into a third one.
  --
  --  Used for:
  --    - informing GL about reduced visible area (scene visible through a portal)
  --    - skipping rendering of scenes on empty rectangles.
  --
  procedure Intersect (A, B : Rectangle; C : out Rectangle; non_empty : out Boolean);

  --  Find the smallest rectangle on screen in which the object will be displayed.
  --
  procedure Find_Bounding_Box
    (o       : in     Object_3D'Class;
     face    : in     Positive;
     b       :    out Rectangle;
     success :    out Boolean);

  --  The following procedures are for experimentation or visual debugging:
  --
  --    - display the current 2D bounding box (clip) for clipping
  --    - display a face's (typically a portal's) edges in the 3D scene.

  procedure Draw_Boundary (main, clip : Rectangle; portal_depth : Natural := 0);

  procedure Show_Edges
    (o            : Object_3D'Class;
     face         : Positive;
     clip         : Clipping_Data;
     portal_depth : Natural := 0);

end GLOBE_3D.Portals;
