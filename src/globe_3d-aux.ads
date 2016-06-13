--  Various helpers or support functions.

package GLOBE_3D.Aux is

  --  Indicate a texture's name that can be resolved later by Rebuild_links
  procedure Texture_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for texture
  );

  --  Indicate a specular map's name that can be resolved later by Rebuild_links
  procedure Specular_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for texture
  );

  --  Indicate a portal's name that can be resolved later by Rebuild_links
  procedure Portal_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for connected object
  );

  --  Blending support
  --
  function Is_to_blend (m: GL.Double)                  return Boolean;
  function Is_to_blend (m: GL.Float)                   return Boolean;
  function Is_to_blend (m: GL.Material_Float_vector)   return Boolean;
  function Is_to_blend (m: GL.Materials.Material_type) return Boolean;

  --  Misc.

  function Image( r: Real ) return String;

  function Coords( p: Point_3D ) return String;

  procedure Angles_modulo_360( v: in out Vector_3D );

  --  !!! [finish body] function Simplify(o: Object_3D) return Object_3D;

end GLOBE_3D.Aux;
