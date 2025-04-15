with GL.Simple_Text, GL.Math, GLU;

package body GLOBE_3D.Portals is

  --  Cheap but fast portal culling method with rectangles.

  procedure Intersect (A, B : Rectangle; C : out Rectangle; non_empty : out Boolean) is
  begin
    C.X1 := Integer'Max (A.X1, B.X1);
    C.X2 := Integer'Min (A.X2, B.X2);
    C.Y1 := Integer'Max (A.Y1, B.Y1);
    C.Y2 := Integer'Min (A.Y2, B.Y2);
    non_empty := C.X1 <= C.X2 and C.Y1 <= C.Y2;
  end Intersect;

  procedure Projection
    (P       : in  Point_3D;
     x, y    : out Integer;
     success : out Boolean)
  is
    model, proj : GLU.doubleMatrix;
    view : GLU.viewPortRec;
    uu, vv, ww : GL.Double;
    use GL;
    z_a : constant := 0.0001;
    z_z : constant := 1.0 - z_a;
    --  GLU sometimes gives fuzzy (e.g. -3612 instead of +4243)
    --  x,y coordinates, with z outside ]0;1[; looks like a wrap-around
    --  of large integer values
  begin
    GLU.Get (GL.MODELVIEW_MATRIX, model);
    GLU.Get (GL.PROJECTION_MATRIX, proj);
    GLU.Get (view);
    GLU.Project (P (0), P (1), P (2), model, proj, view, uu, vv, ww, success);
    success :=
        success
      and then
        ww > z_a
      and then
        ww < z_z;

    if success then
      x := Integer (uu);
      y := Integer (vv);
      --  info_a_pnt(0):= (uu,vv,ww);
    else
      x := -1;
      y := -1;
    end if;
  end Projection;

  procedure Find_Bounding_Box
    (o       : in     Object_3D'Class;
     face    : in     Positive;
     b       :    out Rectangle;
     success :    out Boolean)
  is
    x, y : Integer;
    proj_success : Boolean;
    use GL.Math;
  begin
    b := (X1 | Y1 => Integer'Last, X2 | Y2 => Integer'First);

    for sf in reverse 1 .. o.face_internal (face).last_edge loop
      Projection
        (o.point (o.face_internal (face).P_compact (sf)) + o.centre,
         x, y,
         proj_success);

      if proj_success then
        --  info_a_pnt(sf):= info_a_pnt(0);
        b.X1 := Integer'Min (b.X1, x);
        b.X2 := Integer'Max (b.X2, x);
        b.Y1 := Integer'Min (b.Y1, y);
        b.Y2 := Integer'Max (b.Y2, y);
      else
        --  We were unable to project all edges of the polygon.
        success := False;
        return;
      end if;
    end loop;
    success := True;
  end Find_Bounding_Box;

  procedure Draw_Boundary (main, clip : Rectangle; portal_depth : Natural := 0) is
    use GL, REF;

    procedure Line (x1, y1, x2, y2 : Integer) is
    begin
      Vertex (GL.Double (x1), GL.Double (y1), 0.0);
      Vertex (GL.Double (x2), GL.Double (y2), 0.0);
    end Line;

    procedure Frame_Rect (x1, y1, x2, y2 : Integer) is
    begin
      Line (x1, y1, x2, y1);
      Line (x2, y1, x2, y2);
      Line (x2, y2, x1, y2);
      Line (x1, y2, x1, y1);
    end Frame_Rect;

    rect : Rectangle;
    val : constant Real := 0.3 + 0.7 * Exp (-GL.Double (portal_depth));
    text_size : constant := 12.5;

  begin
    GL.Disable (GL.Lighting);
    GL.Disable (GL.Texture_2D);
    --  GL.Disable( GL.DEPTH_TEST ); -- eeerh, @#*$!, doesn't work!
    --  Workaround, we make the rectangle 1 pixel smaller
    rect := (clip.X1 + 1, clip.Y1 + 1, clip.X2 - 1, clip.Y2 - 1);
    --  Push current matrix mode and viewport attributes.
    GL.PushAttrib (GL.TRANSFORM_BIT + GL.VIEWPORT_BIT);
    GL.MatrixMode (GL.PROJECTION);
    GL.PushMatrix;
    GL.LoadIdentity;
    GL.Ortho
      (left_val   => 0.0,
       right_val  => GL.Double (main.X2 - 1),
       bottom_val => 0.0,
       top_val    => GL.Double (main.Y2 - 1),
       near_val   => -1.0,
       far_val    =>  1.0);

    GL.MatrixMode (GL.MODELVIEW);
    GL.PushMatrix;
    GL.LoadIdentity;

    --  Portal label:
    GL.Simple_Text.Text_Output
      ((GL.Double (clip.X1),  GL.Double (clip.Y2) - text_size, 0.0),
       "Portal depth:" & portal_depth'Image,
       (0.7, 0.7, val, 0.9),
       text_size,
       GL.Simple_Text.Sans_Serif);

    --  A green rectangle (2D) to signal the clipping area:
    GL.Color (0.1, val, 0.1, 1.0);
    GL_Begin (GL.LINES);
    Frame_Rect (rect.X1, rect.Y1, rect.X2, rect.Y2);
    GL_End;
    --  A red cross (2D) across the area:
    GL.Color (val, 0.1, 0.1, 1.0);
    GL_Begin (GL.LINES);
    Line (clip.X1, clip.Y1, clip.X2, clip.Y2);
    Line (clip.X2, clip.Y1, clip.X1, clip.Y2);
    GL_End;

    GL.PopMatrix;
    GL.MatrixMode (GL.PROJECTION);
    GL.PopMatrix;
    GL.PopAttrib;
    GL.Enable (GL.Lighting);
    --  GL.Enable( GL.DEPTH_TEST );

  end Draw_Boundary;

  --  Temporary location for a future Simple Shapes
  --
  function Cylinder
    (bottom_center : Point_3D;  --  Center of bottom disc
     top_center    : Point_3D;  --  Center of top disc
     radius        : Real;
     slices        : Positive;
     stacks        : Positive;
     colour        : GL.RGB_Color)
     return Object_3D
  is
    use GL, GL.Math, REF;
    res : Object_3D (Max_points => (1 + stacks) * slices, Max_faces => stacks * slices);
    null_object : Object_3D (0, 0);
    main_axis   : constant Vector_3D := top_center - bottom_center;
    main_length : constant Real := Norm (main_axis);
    xyz, uvw, efg : Vector_3D;  --  3 orthogonal unit vectors; xyz is parallel to main axis.
    x, y, z, len_xy, inv_len_xy : Real;
    a0, b0, a, b, slice_angle, c, s : Real;
    stack_center : Point_3D;
    face : Face_Type;
  begin
    if Almost_Zero (main_length) then
      return null_object;
    end if;
    xyz := (1.0 / main_length) * main_axis;
    x := xyz (0);
    y := xyz (1);
    z := xyz (2);
    --  We define a simple vector, orthogonal to the main axis.
    if Almost_Zero (x) then
      uvw := (1.0, 0.0, 0.0);
      efg := (0.0,   z,  -y);
    else
      len_xy := Sqrt (x**2 + y**2);
      inv_len_xy := 1.0 / len_xy;
      uvw := inv_len_xy * (y, -x, 0.0);
      efg := (inv_len_xy * x * z, inv_len_xy * y * z, -len_xy);
    end if;

    a := radius;
    b := 0.0;
    slice_angle := 2.0 * Pi / Real (slices);
    c := Cos (slice_angle);
    s := Sin (slice_angle);
    for slice in 1 .. slices loop
      a0 := a;
      b0 := b;
      --  We rotate the 2D (a, b) point.
      a := c * a0 - s * b0;
      b := s * a0 + c * b0;
      for stack in 0 .. stacks loop
        stack_center := bottom_center + Real (stack) / Real (stacks) * main_axis;
        res.point (stack * slices + slice) :=
           stack_center + a0 * uvw + b0 * efg;
      end loop;
      for stack in 0 .. stacks - 1 loop
        face.P :=
          (stack * slices       + slice,      --  Face n begins with point n.
           stack * slices       + slice + 1,  --  Next point in the same disk.
           (stack + 1) * slices + slice + 1,  --  Point on the disk "above" the previous point.
           (stack + 1) * slices + slice);     --  Point "above" the first point.
        face.skin   := texture_only;
        face.colour := colour;
        res.face (stack * slices + slice) := face;
      end loop;
    end loop;
    return res;
  end Cylinder;

  procedure Show_Edges
    (o            : Object_3D'Class;
     face         : Positive;
     portal_depth : Natural := 0)
  is
    use GL, GL.Math, REF;
    val : constant Real := 0.3 + 0.7 * Exp (-GL.Double (portal_depth));

    procedure Face_Vertex (v : Positive) is
    begin
      Vertex (o.point (o.face_internal (face).P_compact (v)) + o.centre);
    end Face_Vertex;

  begin
    GL.Disable (GL.Lighting);
    GL.Disable (GL.Texture_2D);
    GL.Disable (GL.Depth_Test);

    --  Signal the portal as a blue polygon (3D):
    GL.Color (0.1, 0.1, val, 1.0);
    GL_Begin (GL.LINES);
    for sf in 1 .. o.face_internal (face).last_edge loop
      Face_Vertex (sf);
    end loop;
    Face_Vertex (1);  --  Complete the polygon.
    GL_End;
    GL.Enable (GL.Lighting);
    GL.Enable (GL.Depth_Test);
  end Show_Edges;

end GLOBE_3D.Portals;
