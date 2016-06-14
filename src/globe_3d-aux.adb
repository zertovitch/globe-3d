with GL.Math, GLOBE_3D.Math;

package body GLOBE_3D.Aux is

  use GL, GL.Math;

  subtype Tri_count is Integer range 1..3;
  subtype Zero_or_tri_count is Integer range 0..3;

  function Is_right_angled_triangle(
    o        : Object_3D;
    face_num : Positive
  )
  return Zero_or_tri_count
  is
    P1, P2, P3: Point_3D;
  begin
    if o.face_internal(face_num).last_edge = 4 then
      return 0;
    end if;
    P1:= o.point(o.face_internal(face_num).P_compact(1));
    P2:= o.point(o.face_internal(face_num).P_compact(2));
    P3:= o.point(o.face_internal(face_num).P_compact(3));
    --
    if    Almost_zero((P2-P1) * (P3-P1)) then
      return 1;
    elsif Almost_zero((P1-P2) * (P3-P2)) then
      return 2;
    elsif Almost_zero((P2-P3) * (P1-P3)) then
      return 3;
    else
      return 0;
    end if;
  end Is_right_angled_triangle;

  --  VR----- V2
  --    |  /|
  --    | / |
  --    |/__|
  --  V1     Point returned
  --
  function Rectangle_completion(
    o           : Object_3D;
    face_num    : Positive;
    right_angle : Tri_count
  )
  return Point_3D
  is
    use GLOBE_3D.Math;
    first: Boolean:= True;
    C, P, V1, V2, VR: Point_3D;
    u: constant Vector_3D:= o.face_internal(face_num).normal;
    --  R: Rotate around axis = face's normal
    R: constant Matrix_33:=
      ( (2.0*u(0)*u(0) - 1.0, 2.0*u(1)*u(0),       2.0*u(2)*u(0)      ),
        (2.0*u(0)*u(1)      , 2.0*u(1)*u(1) - 1.0, 2.0*u(2)*u(1)      ),
        (2.0*u(0)*u(2)      , 2.0*u(1)*u(2)      , 2.0*u(2)*u(2) - 1.0) );
  begin
    for i in Tri_count loop
      P:= o.point(o.face_internal(face_num).P_compact(i));
      if i = right_angle then
        VR:= P;
      elsif first then
        V1:= P;
        first:= False;
      else
        V2:= P;
      end if;
    end loop;
    C:= 0.5 * (V1 + V2);
    return C + R * (VR - C);
  end Rectangle_completion;

  --  Same for texture coordinates
  --  Assumption: textured skin, o.face.whole_texture(face_num) = False

  --  VR----- V2
  --    |  /|
  --    | / |
  --    |/__|
  --  V1     Point returned
  --
  function Rectangle_completion(
    o           : Object_3D;
    face_num    : Positive;
    right_angle : Tri_count
  )
  return Map_idx_pair
  is
    use GLOBE_3D.Math;
    first: Boolean:= True;
    C, P, V1, V2, VR: Map_idx_pair;
  begin
    for i in Tri_count loop
      P:= o.face_internal(face_num).UV_extrema(i);
      if i = right_angle then
        VR:= P;
      elsif first then
        V1:= P;
        first:= False;
      else
        V2:= P;
      end if;
    end loop;
    C:= 0.5 * (V1 + V2);
    return C + (-1.0) * (VR - C);
  end Rectangle_completion;

  --  -----
  --  |A /|
  --  | /B|
  --  |/__|
  --
  function Matching_right_angled_triangles(
    o        : Object_3D;
    fa, fb   : Positive;   --  Face index of both right-angled triangles
    raa, rab : Tri_count   --  Right angle vertex index of both triangles in P_compact arrays.
  )
  return Boolean
  is
    use GL.Materials;
    --  !! We might relax the condition of Match_point
    function Match_point(P1, P2: Point_3D) return Boolean renames GL.Math.Identical;
    function Match_vector(P1, P2: Vector_3D) return Boolean renames GL.Math.Identical;
    match_count: Natural:= 0;
  begin
    if not Match_vector(o.face_internal(fa).normal, o.face_internal(fb).normal) then
      return False;
    end if;
    if o.face(fa).skin /= o.face(fb).skin then
      return False;
    end if;
    --  We look if two vertices, not with the right angle, of rectangle A match
    --  two vertices of rectangle B (again, not with the right angle)
    for ia in Tri_count loop
      if ia /= raa then
        for ib in Tri_count loop
          if ib /= rab then
            if Match_point(
              o.point(o.face_internal(fa).P_compact(ia)),
              o.point(o.face_internal(fb).P_compact(ib))
            )
            then
              match_count:= match_count + 1;
            end if;
          end if;
        end loop;
      end if;
    end loop;
    if match_count /= 2 then  --  if match_count = 3 or 4, we have 1 or 2 degenerated triangles.
      return False;
    end if;
    if not Match_point(
      Rectangle_completion(o, fa, raa),
      o.point(o.face_internal(fb).P_compact(rab))
    )
    then
      return False;
    end if;
    --  At this point, triangles can be merged geometrically and have the same skin type.
    --  We check if they are compatible.
    if o.face(fa).skin = invisible then
      return True;
    end if;
    if is_textured(o.face(fa).skin) then
      if o.face(fa).texture = null_image then
        --  Names not yet resolved
        if o.face_internal(fa).texture_name  /= o.face_internal(fb).texture_name or else
           o.face_internal(fa).specular_name /= o.face_internal(fb).specular_name
        then
          return False;
        end if;
      else
        if o.face(fa).texture      /= o.face(fb).texture or else
           o.face(fa).specular_map /= o.face(fb).specular_map
        then
          return False;
        end if;
      end if;
      --  Now we check if the points in texture coordinates would match
      --  !! use almost_zero match
      if Rectangle_completion(o, fa, raa) /= o.face_internal(fb).UV_extrema(rab) then
        return False;  --  Picture wouldn't match the one of both triangles.
      end if;
    end if;
    --  !! use almost_zero match
    if is_coloured(o.face(fa).skin) then
      if o.face(fa).colour /= o.face(fb).colour or else
         o.face(fa).alpha  /= o.face(fb).alpha
      then
        return False;
      end if;
    end if;
    --  !! use almost_zero match
    if is_material(o.face(fa).skin) then
      if o.face(fa).material /= o.face(fb).material then
        return False;
      end if;
    end if;
    return True;
  end Matching_right_angled_triangles;

  function Merge_triangles(obj: Object_3D) return Object_3D is
    o: Object_3D:= obj;  --  Clone, for pre-calculating if needed.
    right_angled_triangle_idx_0123: array(1..o.Max_faces) of Zero_or_tri_count;
    matching_index: array(1..o.Max_faces) of Natural:= (others => 0);
    matched: array(1..o.Max_faces) of Boolean:= (others => False);
    raa, rab: Zero_or_tri_count;
    face_reduction: Natural:= 0;
  begin
    if not o.pre_calculated then
      o.Pre_calculate;
    end if;
    --  First, flag all right-angled triangles.
    for f in 1..o.Max_faces loop
      right_angled_triangle_idx_0123(f):= Is_right_angled_triangle(o, f);
    end loop;
    --  Find matching pairs of right-angled triangles.
    for fa in 1..o.Max_faces loop
      raa:= right_angled_triangle_idx_0123(fa);
      if raa > 0 then
        for fb in fa+1..o.Max_faces loop
          rab:= right_angled_triangle_idx_0123(fb);
          if rab > 0 then
            if Matching_right_angled_triangles(o, fa, fb, raa, rab) then
              --  fa will become a rectangle in new object.
              --  fb will be ignored in new object.
              matching_index(fa):= fb;
              matched(fb):= True;
              face_reduction:= face_reduction + 1;
            end if;
          end if;
        end loop;
      end if;
    end loop;
    --  Build compacted object
    declare
      res: Object_3D( Max_points=> o.Max_points, Max_faces=> o.Max_faces - face_reduction );
      nf: Natural:= 0;
      fb: Natural;
      new_vertex_id: Positive;
    begin
      --  Clone basic features
      res.ID     := o.ID;
      res.centre := o.centre;
      res.point  := o.point;
      for f in 1..o.Face_Count loop
        if matched(f) then
          null;  --  skip this face
        else
          nf:= nf + 1;
          --  Clone face features
          res.face(nf):= o.face(f);
          res.face_internal(nf):= o.face_internal(f);
          fb:= matching_index(f);
          if fb > 0 then
            --  We transform the triangle into a rectangle, taking the extra vertex from fb.
            rab:= right_angled_triangle_idx_0123(fb);
            new_vertex_id:= o.face_internal(fb).P_compact(rab);
            for i in 1..4 loop
              if res.face(nf).P(i) = 0 then  --  Here is the "blind edge"
                res.face(nf).P(i):= new_vertex_id;
                if is_textured(res.face(nf).skin) then
                  if res.face(nf).whole_texture then
                    null;  --  Nothing to do, edges calculated in Calculate_face_internals
                  else
                    --  Shift (shouldn't happen, i = 4 here, with whole_texture = False)
                    for k in reverse i .. 3 loop
                      res.face(nf).texture_edge_map(k+1):=
                      res.face(nf).texture_edge_map(k);
                    end loop;
                    res.face(nf).texture_edge_map(i):= o.face_internal(fb).UV_extrema(rab);
                  end if;
                end if;
              end if;
            end loop;
          end if;
        end if;
      end loop;
      res.Pre_calculate;
      return res;
    end;
  end Merge_triangles;

  procedure Set_ident(i: out Ident; name: String)
  is
  begin
    if name'Length > Ident'Length then
      raise Constraint_Error with "Name too long for fixed-length type Ident";
    end if;
    i:= empty;  --  Stuff with blanks.
    i(1..name'Length):= name;
  end Set_ident;

  procedure Texture_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for texture
  )
  is
  begin
    Set_ident(o.face_internal(face).texture_name, name);
  end Texture_name_hint;

  procedure Specular_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for texture
  )
  is
  begin
    Set_ident(o.face_internal(face).specular_name, name);
  end Specular_name_hint;

  procedure Portal_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for connected object
  )
  is
  begin
    Set_ident(o.face_internal(face).connect_name(1..name'Length), name);
  end Portal_name_hint;

  function Image( r: Real ) return String is
    s: String(1..10);
  begin
    RIO.Put(s,r,4,0);
    return s;
  exception
    when Ada.Text_IO.Layout_Error =>
      return Real'Image(r);
  end Image;

  function Coords( p: Point_3D ) return String is
  begin
    return '(' & Image(p(0)) &
           ',' & Image(p(1)) &
           ',' & Image(p(2)) &
           ')';
  end Coords;

  prec_a360    : constant:= 10000;
  r_prec_a360  : constant:= 10000.0;
  i_r_prec_a360: constant:= 1.0 / r_prec_a360;

  procedure Angles_modulo_360( v: in out Vector_3D )is
  begin
    for i in v'Range loop
      v(i):=
        GL.Double(Integer(r_prec_a360 * v(i)) mod (360*prec_a360))
        * i_r_prec_a360;
    end loop;
  end Angles_modulo_360;

  --  Blending support
  --

  function Is_to_blend(m: GL.Double) return Boolean is
  begin
    return not Almost_zero(m-1.0);
  end Is_to_blend;

  function Is_to_blend(m: GL.Float) return Boolean is
  begin
    return not Almost_zero(m-1.0);
  end Is_to_blend;

  function Is_to_blend(m: GL.Material_Float_vector) return Boolean is
  begin
    return Is_to_blend(m(3));
  end Is_to_blend;

  function Is_to_blend(m: GL.Materials.Material_type) return Boolean  is
  begin
    return
      Is_to_blend(m.ambient) or
      Is_to_blend(m.diffuse) or
      Is_to_blend(m.specular);
      -- m.emission, m.shininess not relevant
  end Is_to_blend;

end GLOBE_3D.Aux;
