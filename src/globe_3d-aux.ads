--  Various helpers.

package GLOBE_3D.Aux is

  -- Indicate a texture's name that can be resolved later by Rebuild_links
  procedure Texture_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for texture
  );

  -- Indicate a specular map's name that can be resolved later by Rebuild_links
  procedure Specular_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for texture
  );

  -- Indicate a portal's name that can be resolved later by Rebuild_links
  procedure Portal_name_hint(
    o   : in out Object_3D'Class;
    face:        Positive;
    name:        String  --  give name as hint for connected object
  );

  function Image( r: Real ) return String;

  function Coords( p: Point_3D ) return String;

  procedure Angles_modulo_360( v: in out Vector_3D );

end GLOBE_3D.Aux;
