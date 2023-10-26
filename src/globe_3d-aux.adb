with GL.Math, GLOBE_3D.Math;

package body GLOBE_3D.Aux is

  use GL, GL.Math;

  subtype Tri_count is Integer range 1 .. 3;
  subtype Zero_or_tri_count is Integer range 0 .. 3;

  function Is_right_angled_triangle (o : Object_3D; face_num : Positive)
  return Zero_or_tri_count
  is
    P1, P2, P3 : Point_3D;
  begin
    if o.face_internal (face_num).last_edge = 4 then
      return 0;
    end if;
    P1 := o.point (o.face_internal (face_num).P_compact (1));
    P2 := o.point (o.face_internal (face_num).P_compact (2));
    P3 := o.point (o.face_internal (face_num).P_compact (3));
    --
    if    Almost_Zero ((P2 - P1) * (P3 - P1)) then
      return 1;
    elsif Almost_Zero ((P1 - P2) * (P3 - P2)) then
      return 2;
    elsif Almost_Zero ((P2 - P3) * (P1 - P3)) then
      return 3;
    else
      return 0;
    end if;
  end Is_right_angled_triangle;

  --  VR----- V2
  --    |  /|
  --    | / |
  --    |/__|
  --  V1      Point returned
  --
  function Rectangle_Completion
    (o           : Object_3D;
     face_num    : Positive;
     right_angle : Tri_count)
  return Point_3D
  is
    use GLOBE_3D.Math;
    first : Boolean := True;
    C, P, V1, V2, VR : Point_3D;
    u : constant Vector_3D := o.face_internal (face_num).normal;
    --  R: Rotate half-turn around axis = face's normal
    R : constant Matrix_33 :=
       ((2.0 * u (0) * u (0) - 1.0, 2.0 * u (1) * u (0),       2.0 * u (2) * u (0)),
        (2.0 * u (0) * u (1),       2.0 * u (1) * u (1) - 1.0, 2.0 * u (2) * u (1)),
        (2.0 * u (0) * u (2),       2.0 * u (1) * u (2),       2.0 * u (2) * u (2) - 1.0));
  begin
    for i in Tri_count loop
      P := o.point (o.face_internal (face_num).P_compact (i));
      if i = right_angle then
        VR := P;
      elsif first then
        V1 := P;
        first := False;
      else
        V2 := P;
      end if;
    end loop;
    C := 0.5 * (V1 + V2);
    return C + R * (VR - C);
  end Rectangle_Completion;

  --  Same for texture coordinates
  --  Assumption: textured skin, o.face.whole_texture(face_num) = False

  --  VR----- V2
  --    |  /|
  --    | / |
  --    |/__|
  --  V1      Point returned
  --
  function Rectangle_Completion
    (o           : Object_3D;
     face_num    : Positive;
     right_angle : Tri_count)
  return Map_Idx_Pair
  is
    first : Boolean := True;
    C, P, V1, V2, VR : Map_Idx_Pair;
  begin
    for i in Tri_count loop
      P := o.face_internal (face_num).UV_extrema (i);
      if i = right_angle then
        VR := P;
      elsif first then
        V1 := P;
        first := False;
      else
        V2 := P;
      end if;
    end loop;
    C := 0.5 * (V1 + V2);
    return C + (-1.0) * (VR - C);
  end Rectangle_Completion;

  --  -----
  --  |A /|
  --  | /B|
  --  |/__|
  --
  function Matching_right_angled_triangles
    (o        : Object_3D;
     fa, fb   : Positive;   --  Face index of both right-angled triangles
     raa, rab : Tri_count)  --  Right angle vertex index of both triangles in P_compact arrays.
  return Boolean
  is
    --  Conditions with numerical identity
    function Match_Point  (P1, P2 : Point_3D)  return Boolean renames GL.Math.Identical;
    function Match_Vector (P1, P2 : Vector_3D) return Boolean renames GL.Math.Identical;
    function Match_UV (P1, P2 : Map_Idx_Pair) return Boolean renames GLOBE_3D.Identical;
    function Match_Color (P1, P2 : RGB_Color) return Boolean renames GL.Math.Identical;

    function Match_Alpha (a1, a2 : GL.Double) return Boolean is
    begin
      return GL.Math.Almost_Zero (a1 - a2);
    end Match_Alpha;

    function Match_Material (M1, M2 : GL.Materials.Material_Type) return Boolean
      renames GL.Materials.Identical;
    match_count : Natural := 0;
  begin
    if not Match_Vector (o.face_internal (fa).normal, o.face_internal (fb).normal) then
      return False;
    end if;
    if o.face (fa).skin /= o.face (fb).skin then
      return False;
    end if;
    --  We look if two vertices, not with the right angle, of rectangle A match
    --  two vertices of rectangle B (again, not with the right angle)
    for ia in Tri_count loop
      if ia /= raa then
        for ib in Tri_count loop
          if ib /= rab then
            if Match_Point
              (o.point (o.face_internal (fa).P_compact (ia)),
               o.point (o.face_internal (fb).P_compact (ib)))
            then
              match_count := match_count + 1;
            end if;
          end if;
        end loop;
      end if;
    end loop;
    if match_count /= 2 then  --  if match_count = 3 or 4, we have 1 or 2 degenerated triangles.
      return False;
    end if;
    if not Match_Point
      (Rectangle_Completion (o, fa, raa),
       o.point (o.face_internal (fb).P_compact (rab)))
    then
      return False;
    end if;
    --  At this point, triangles can be merged geometrically and have the same skin type.
    --  We check if they are compatible.
    if o.face (fa).skin = invisible then
      return True;
    end if;
    if is_textured (o.face (fa).skin) then
      if o.face (fa).texture = null_image then
        --  Names not yet resolved
        if o.face_internal (fa).texture_name  /= o.face_internal (fb).texture_name or else
           o.face_internal (fa).specular_name /= o.face_internal (fb).specular_name
        then
          return False;
        end if;
      else
        if o.face (fa).texture      /= o.face (fb).texture or else
           o.face (fa).specular_map /= o.face (fb).specular_map
        then
          return False;
        end if;
      end if;
      --  Now we check if the points in texture coordinates would match
      if not
        Match_UV (Rectangle_Completion (o, fa, raa), o.face_internal (fb).UV_extrema (rab))
      then
        return False;  --  Picture would not match the one of both triangles.
      end if;
    end if;
    if is_coloured (o.face (fa).skin) then
      if (not Match_Color (o.face (fa).colour, o.face (fb).colour)) or else
         (not Match_Alpha (o.face (fa).alpha,  o.face (fb).alpha))
      then
        return False;
      end if;
    end if;
    if is_material (o.face (fa).skin) then
      if not Match_Material (o.face (fa).material, o.face (fb).material) then
        return False;
      end if;
    end if;
    return True;
  end Matching_right_angled_triangles;

  function Merge_Triangles (obj : Object_3D) return Object_3D is
    o : Object_3D := obj;  --  Clone, for pre-calculating if needed.
    right_angled_triangle_idx_0123 : array (1 .. o.Max_faces) of Zero_or_tri_count;
    matching_index : array (1 .. o.Max_faces) of Natural := (others => 0);
    matched : array (1 .. o.Max_faces) of Boolean := (others => False);
    raa, rab : Zero_or_tri_count;
    face_reduction : Natural := 0;
  begin
    if not o.pre_calculated then
      o.Pre_Calculate;
    end if;
    --  First, flag all right-angled triangles.
    for f in 1 .. o.Max_faces loop
      right_angled_triangle_idx_0123 (f) := Is_right_angled_triangle (o, f);
    end loop;
    --  Find matching pairs of right-angled triangles.
    for fa in 1 .. o.Max_faces loop
      raa := right_angled_triangle_idx_0123 (fa);
      if raa > 0 then
        for fb in fa + 1 .. o.Max_faces loop
          rab := right_angled_triangle_idx_0123 (fb);
          if rab > 0 then
            if Matching_right_angled_triangles (o, fa, fb, raa, rab) then
              if matched (fb) then
                null;  --  Triangle fb has already been matched.
              else
                --  fa will become a rectangle in new object.
                --  fb will be ignored in new object.
                matching_index (fa) := fb;
                matched (fb) := True;
                face_reduction := face_reduction + 1;
                exit;
              end if;
            end if;
          end if;
        end loop;
      end if;
    end loop;
    --  Build compacted object
    declare
      res : Object_3D (Max_points => o.Max_points, Max_faces => o.Max_faces - face_reduction);
      nf : Natural := 0;
      fb : Natural;
      ra : Tri_count;
      new_vertex_id : Positive;
      new_UV : Map_Idx_Pair;
      index_aux : Positive;
    begin
      --  Clone basic features
      res.ID          := o.ID;
      res.centre      := o.centre;
      res.point       := o.point;
      res.sub_objects := o.sub_objects;
      for f in 1 .. o.Max_faces loop
        if matched (f) then
          null;  --  skip this face
        else
          nf := nf + 1;
          --  Clone face features
          res.face (nf) := o.face (f);
          res.face_internal (nf) := o.face_internal (f);
          fb := matching_index (f);
          if fb > 0 then
            --  We transform the triangle into a rectangle, taking the extra vertex from fb.
            ra := right_angled_triangle_idx_0123 (f);
            rab := right_angled_triangle_idx_0123 (fb);
            new_vertex_id := o.face_internal (fb).P_compact (rab);
            new_UV := o.face_internal (fb).UV_extrema (rab);
            --  Now, we need to have a correct orientation for the new rectangle.
            --  It depends on which vertex has the right angle.
            --  The new vertex need to be on the other side, and you need to take everybody
            --  without making a 'Z'.
            --  Ouch! But with a drawing it's easy...
            case ra is
              when 1 =>  --  New vertex to be inserted between 2 and 3. 1 and 2 are ok.
                res.face (nf).P (1) := o.face_internal (f).P_compact (1);
                res.face (nf).P (2) := o.face_internal (f).P_compact (2);
                res.face (nf).P (3) := new_vertex_id;
                res.face (nf).P (4) := o.face_internal (f).P_compact (3);
              when 2 =>  --  New vertex to be inserted after 3.
                res.face (nf).P (1) := o.face_internal (f).P_compact (1);
                res.face (nf).P (2) := o.face_internal (f).P_compact (2);
                res.face (nf).P (3) := o.face_internal (f).P_compact (3);
                res.face (nf).P (4) := new_vertex_id;
              when 3 =>  --  New vertex to be inserted between 1 and 2.
                res.face (nf).P (1) := o.face_internal (f).P_compact (1);
                res.face (nf).P (2) := new_vertex_id;
                res.face (nf).P (3) := o.face_internal (f).P_compact (2);
                res.face (nf).P (4) := o.face_internal (f).P_compact (3);
            end case;
            if is_textured (res.face (nf).skin) then
              if res.face (nf).whole_texture then
                --  Edges are calculated in Calculate_face_internals (Pre_calculate).
                --    - either the (0,0) point in texture was with the original triangle,
                --      the P(1) was already = P_compact(1) and is preserved by the above ordering
                --    - or the new edge has the (0,0): then we need to reorder...
                if Identical (new_UV, (0.0, 0.0)) then
                  for count in 1 .. 4 loop
                    --  Rotate the edge indices
                    index_aux := res.face (nf).P (1);
                    res.face (nf).P (1) := res.face (nf).P (2);
                    res.face (nf).P (2) := res.face (nf).P (3);
                    res.face (nf).P (3) := res.face (nf).P (4);
                    res.face (nf).P (4) := index_aux;
                    exit when res.face (nf).P (1) = new_vertex_id;  --  Now (0,0) is on P(1).
                  end loop;
                end if;
              else
                --  Re-ouch!
                case ra is
                  when 1 =>  --  New vertex to be inserted between 2 and 3. 1 and 2 are ok.
                    res.face (nf).texture_edge_map (1) := o.face_internal (f).UV_extrema (1);
                    res.face (nf).texture_edge_map (2) := o.face_internal (f).UV_extrema (2);
                    res.face (nf).texture_edge_map (3) := new_UV;
                    res.face (nf).texture_edge_map (4) := o.face_internal (f).UV_extrema (3);
                  when 2 =>  --  New vertex to be inserted after 3.
                    res.face (nf).texture_edge_map (1) := o.face_internal (f).UV_extrema (1);
                    res.face (nf).texture_edge_map (2) := o.face_internal (f).UV_extrema (2);
                    res.face (nf).texture_edge_map (3) := o.face_internal (f).UV_extrema (3);
                    res.face (nf).texture_edge_map (4) := new_UV;
                  when 3 =>  --  New vertex to be inserted between 1 and 2.
                    res.face (nf).texture_edge_map (1) := o.face_internal (f).UV_extrema (1);
                    res.face (nf).texture_edge_map (2) := new_UV;
                    res.face (nf).texture_edge_map (3) := o.face_internal (f).UV_extrema (2);
                    res.face (nf).texture_edge_map (4) := o.face_internal (f).UV_extrema (3);
                end case;
              end if;
            end if;
          end if;
        end if;
      end loop;
      res.Pre_Calculate;
      return res;
    end;
  end Merge_Triangles;

  procedure Set_ident (i : out Ident; name : String)
  is
  begin
    if name'Length > Ident'Length then
      raise Constraint_Error with "Name too long for fixed-length type Ident";
    end if;
    i := empty_ident;  --  Stuff with blanks.
    i (1 .. name'Length) := name;
  end Set_ident;

  procedure Texture_Name_Hint
    (o    : in out Object_3D'Class;
     face :        Positive;
     name :        String)  --  give name as hint for texture
  is
  begin
    Set_ident (o.face_internal (face).texture_name, name);
  end Texture_Name_Hint;

  procedure Specular_Name_Hint
    (o    : in out Object_3D'Class;
     face :        Positive;
     name :        String)  --  give name as hint for texture
  is
  begin
    Set_ident (o.face_internal (face).specular_name, name);
  end Specular_Name_Hint;

  procedure Portal_Name_Hint
    (o    : in out Object_3D'Class;
     face :        Positive;
     name :        String)  --  give name as hint for connected object
  is
  begin
    Set_ident (o.face_internal (face).connect_name (1 .. name'Length), name);
  end Portal_Name_Hint;

  function Image (r : Real) return String is
    s : String (1 .. 10);
  begin
    RIO.Put (s, r, 4, 0);
    return s;
  exception
    when Ada.Text_IO.Layout_Error =>
      return r'Image;
  end Image;

  function Coords (p : Point_3D) return String is
  begin
    return '(' & Image (p (0)) &
           ',' & Image (p (1)) &
           ',' & Image (p (2)) &
           ')';
  end Coords;

  prec_a360     : constant := 10000;
  r_prec_a360   : constant := 10000.0;
  i_r_prec_a360 : constant := 1.0 / r_prec_a360;

  procedure Angles_Modulo_360 (v : in out Vector_3D) is
  begin
    for i in v'Range loop
      v (i) :=
        GL.Double (Integer (r_prec_a360 * v (i)) mod (360 * prec_a360))
        * i_r_prec_a360;
    end loop;
  end Angles_Modulo_360;

  --  Blending support
  --

  function Is_to_blend (m : GL.Double) return Boolean is
  begin
    return not Almost_Zero (m - 1.0);
  end Is_to_blend;

  function Is_to_blend (m : GL.Float) return Boolean is
  begin
    return not Almost_Zero (m - 1.0);
  end Is_to_blend;

  function Is_to_blend (m : GL.Material_Float_Vector) return Boolean is
  begin
    return Is_to_blend (m (3));
  end Is_to_blend;

  function Is_to_blend (m : GL.Materials.Material_Type) return Boolean  is
  begin
    return
      Is_to_blend (m.ambient) or
      Is_to_blend (m.diffuse) or
      Is_to_blend (m.specular);
      --  m.emission, m.shininess not relevant
  end Is_to_blend;

end GLOBE_3D.Aux;
