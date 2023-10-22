--  Various helpers or support functions.

package GLOBE_3D.Aux is

  --  Indicate a texture's name that can be resolved later by Rebuild_Links
  procedure Texture_Name_Hint
    (o    : in out Object_3D'Class;
     face :        Positive;
     name :        String);  --  give name as hint for texture

  --  Indicate a specular map's name that can be resolved later by Rebuild_Links
  procedure Specular_Name_Hint
    (o    : in out Object_3D'Class;
     face :        Positive;
     name :        String);  --  give name as hint for texture

  --  Indicate a portal's name that can be resolved later by Rebuild_Links
  procedure Portal_Name_Hint
    (o    : in out Object_3D'Class;
     face :        Positive;
     name :        String);  --  give name as hint for connected object

  --  Blending support
  --
  function Is_to_blend (m : GL.Double)                  return Boolean;
  function Is_to_blend (m : GL.Float)                   return Boolean;
  function Is_to_blend (m : GL.Material_Float_vector)   return Boolean;
  function Is_to_blend (m : GL.Materials.Material_type) return Boolean;

  --  Misc.

  function Image (r : Real) return String;

  function Coords (p : Point_3D) return String;

  procedure Angles_Modulo_360 (v : in out Vector_3D);

  function Merge_Triangles (obj : Object_3D) return Object_3D;

end GLOBE_3D.Aux;
