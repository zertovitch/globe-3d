package body GLOBE_3D.Aux is

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
    use GL;
  begin
    for i in v'Range loop
      v(i):=
        GL.Double(Integer(r_prec_a360 * v(i)) mod (360*prec_a360))
        * i_r_prec_a360;
    end loop;
  end Angles_modulo_360;

end GLOBE_3D.Aux;
