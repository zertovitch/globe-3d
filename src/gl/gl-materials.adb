with GL.Math;

package body GL.Materials is

   function Is_transparent (Self : in Material_type) return Boolean
   is
   begin
      return Self.diffuse (3) < 1.0;
   end Is_transparent;

  function Identical (a, b: Material_Float_vector) return Boolean is
    use GL.Math;
  begin
    return
      Almost_zero (a(0)-b(0)) and then
      Almost_zero (a(1)-b(1)) and then
      Almost_zero (a(2)-b(2)) and then
      Almost_zero (a(3)-b(3));
  end Identical;

  function Identical (m1, m2: Material_type) return Boolean is
    use GL.Math;
  begin
    return
      Identical (m1.ambient,  m2.ambient)  and then
      Identical (m1.diffuse,  m2.diffuse)  and then
      Identical (m1.specular, m2.specular) and then
      Identical (m1.emission, m2.emission) and then
      Almost_zero (m1.shininess - m2.shininess);
  end Identical;

  procedure Set_Material (m: GL.Materials.Material_type) is
  begin
     Material (Front_And_Back, AMBIENT,   m.ambient);
     Material (Front_And_Back, DIFFUSE,   m.diffuse);
     Material (Front_And_Back, SPECULAR,  m.specular);
     Material (Front_And_Back, EMISSION,  m.emission);
     Material (Front_And_Back, SHININESS, m.shininess);
  end Set_Material;

end GL.Materials;
