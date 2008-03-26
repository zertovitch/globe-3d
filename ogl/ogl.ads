
with GL;
with Math;



package oGL is
   --
   -- a thick binding to the openGL API.

   --pragma Pure;

   use GL;



  type Rectangle is record X1,Y1,X2,Y2: Integer; end record;

  subtype Clipping_area is Rectangle;

  -- ^ Cheap but fast portal culling & clipping method with rectangles.
  --   Usually, a bit too much is displayed.
  --   With graphics cards as of 2005+, it doesn't matter at all
  --   The important aspect is the culling of the objects when the
  --   intersection is empty.

  type Clipping_data is record
    eye_position    : aliased math.Vector_3;
    view_direction  :         math.Vector_3;
    max_dot_product :         math.Real;         -- depends on the field of view
    main_clipping   :         Clipping_area;
  end record;


end oGL;
