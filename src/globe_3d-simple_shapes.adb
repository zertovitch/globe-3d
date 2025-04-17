with GL.Math;

package body GLOBE_3D.Simple_Shapes is

  function Cylinder
    (disk_center_1 : Point_3D;  --  Center of "bottom" disc
     disk_center_2 : Point_3D;  --  Center of "top" disc
     radius        : Real;
     slices        : Positive;
     stacks        : Positive;
     colour        : GL.RGB_Color)
     return Object_3D
  is
  begin
    return Truncated_Cone (disk_center_1, disk_center_2, radius, radius, slices, stacks, colour);
  end Cylinder;

  function Truncated_Cone
    (disk_center_1 : Point_3D;  --  Center of "bottom" disc
     disk_center_2 : Point_3D;  --  Center of "top" disc
     radius_1      : Real;
     radius_2      : Real;
     slices        : Positive;
     stacks        : Positive;
     colour        : GL.RGB_Color)
     return Object_3D
  is
    use GL, GL.Math, REF;
    res : Object_3D (Max_points => (1 + stacks) * slices, Max_faces => stacks * slices);
    null_object : Object_3D (0, 0);
    main_axis   : constant Vector_3D := disk_center_2 - disk_center_1;
    main_length : constant Real := Norm (main_axis);
    xyz, uvw, efg : Vector_3D;  --  3 orthogonal unit vectors; xyz is parallel to main axis.
    x, y, z, len_xy, inv_len_xy, h, radius : Real;
    a0, b0, a, b, slice_angle, c, s : Real;
    stack_center : Point_3D;
    face : Face_Type;
    next : Positive;
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

    a := 1.0;
    b := 0.0;
    slice_angle := 2.0 * Pi / Real (slices);
    c := Cos (slice_angle);
    s := Sin (slice_angle);
    for slice in 1 .. slices loop
      a0 := a;
      b0 := b;
      --  We rotate the 2D (a, b) point for the circle approximation.
      a := c * a0 - s * b0;
      b := s * a0 + c * b0;
      for stack in 0 .. stacks loop
        h := Real (stack) / Real (stacks);
        stack_center := disk_center_1 + h * main_axis;
        radius := (1.0 - h) * radius_1 + h * radius_2;
        res.point (stack * slices + slice) :=
           stack_center + radius * (a0 * uvw + b0 * efg);
      end loop;
      next := (if slice = slices then 1 else slice + 1);
      for stack in 0 .. stacks - 1 loop
        face.P :=
          (stack * slices       + slice,   --  Face n begins with point n.
           stack * slices       + next,    --  Next point in the same disk.
           (stack + 1) * slices + next,    --  Point on the disk "above" the previous point.
           (stack + 1) * slices + slice);  --  Point "above" the first point.
        face.skin   := colour_only;
        face.colour := colour;
        res.face (stack * slices + slice) := face;
      end loop;
    end loop;
    return res;
  end Truncated_Cone;

end GLOBE_3D.Simple_Shapes;
