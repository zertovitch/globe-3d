with GL.Math;

package body GLOBE_3D.Aux is

  use GL, GL.Math;

  subtype Tri_count is Integer range 1..3;
  subtype Zero_or_tri_count is Integer range 0..3;

  function Is_right_angled_rectangle(
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
    return 0; -- !!
  end Is_right_angled_rectangle;

  function Matching_right_angled_rectangle(
    o        : Object_3D;
    fa, fb   : Positive;   --  Face index of both triangles
    raa, rab : Tri_count   --  Right angle vertex index of both triangles in P_compact arrays.
  )
  return Boolean
  is
    match_count: Natural:= 0;
  begin
    for ia in Tri_count loop
      if ia /= raa then
        for ib in Tri_count loop
          if ib /= rab then
            if Identical(  --  !! We might relax this condition
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
    return False;  -- !!
  end Matching_right_angled_rectangle;

  function Simplify(o: Object_3D) return Object_3D is
    res: Object_3D( Max_points=> 8, Max_faces=> 6 );  -- !!
    right_angled_triangle_flag: array(1..o.Max_faces) of Zero_or_tri_count;
    matching_index: array(1..o.Max_faces) of Natural:= (others => 0);
    raa, rab: Zero_or_tri_count;
    face_reduction: Natural:= 0;
  begin
    --  First, flag all right-angled triangles.
    for f in 1..o.Max_faces loop
      right_angled_triangle_flag(f):= Is_right_angled_rectangle(o, f);
    end loop;
    --  Find matching pairs of right-angled triangles.
    for fa in 1..o.Max_faces loop
      raa:= right_angled_triangle_flag(fa);
      if raa > 0 then
        for fb in fa+1..o.Max_faces loop
          rab:= right_angled_triangle_flag(fb);
          if rab > 0 then
            if Matching_right_angled_rectangle(o, fa, fb, raa, rab) then
              --  fa will become a rectangle in new object.
              --  fb will be ignored in new object.
              matching_index(fa):= fb;
              face_reduction:= face_reduction + 1;
            end if;
          end if;
        end loop;
      end if;
    end loop;
    return res;
  end;

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
