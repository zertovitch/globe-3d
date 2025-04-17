with GLOBE_3D.Simple_Shapes;
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

  procedure Show_Edges
    (o            : Object_3D'Class;
     face         : Positive;
     clip         : Clipping_Data;
     portal_depth : Natural := 0)
  is
    use GL, GL.Math, REF;
    depth_val : constant Real := 0.2 + 0.8 * Exp (-GL.Double (portal_depth));
    deep_blue : constant GL.RGB_Color := (0.05, 0.05, depth_val);

    type Display_Mode is (wires, cylinders);
    mode : constant Display_Mode := cylinders;

    last : constant Integer := o.face_internal (face).last_edge;

    function Face_Vertex (v : Positive) return Point_3D is
    (o.point (o.face_internal (face).P_compact (v)) + o.centre);

    procedure Line_To_Face_Vertex (v : Positive) is
    begin
      GL.Vertex (Face_Vertex (v));
    end Line_To_Face_Vertex;

    p1, p2 : Point_3D;

  begin
    case mode is

      when wires =>
        GL.Disable (GL.Depth_Test);
        --  Signal the portal as a blue polygon (3D):
        GL.Color (deep_blue);
        GL_Begin (GL.LINES);
        for sf in 1 .. last loop
          Line_To_Face_Vertex (sf);
        end loop;
        Line_To_Face_Vertex (1);  --  Complete the polygon.
        GL_End;
        GL.Enable (GL.Depth_Test);

      when cylinders =>
        for sf in 1 .. last loop
          p1 := Face_Vertex ((if sf = 1 then last else sf - 1));
          p2 := Face_Vertex (sf);
          declare
            cyl : Object_3D :=
              Simple_Shapes.Cylinder
                (disk_center_1 => p1,
                 disk_center_2 => p2,
                 radius        => Norm (p2 - p1) * 0.06,
                 slices        => 20,
                 stacks        => 8,
                 colour        => deep_blue);
          begin
            cyl.Display (clip);
          end;
        end loop;

    end case;
  end Show_Edges;

end GLOBE_3D.Portals;
