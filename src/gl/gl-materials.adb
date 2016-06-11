package body GL.Materials is

   function  is_Transparent (Self : in Material_type) return Boolean
   is
   begin
      return Self.diffuse (3) < 1.0;
   end;

  procedure Set_Material(m: GL.Materials.Material_type) is
     use GL;
  begin
     Material(FRONT_AND_BACK, AMBIENT,   m.ambient);
     Material(FRONT_AND_BACK, DIFFUSE,   m.diffuse);
     Material(FRONT_AND_BACK, SPECULAR,  m.specular);
     Material(FRONT_AND_BACK, EMISSION,  m.emission);
     Material(FRONT_AND_BACK, SHININESS, m.shininess);
  end Set_Material;

end GL.Materials;
