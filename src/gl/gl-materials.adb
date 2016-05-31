package body GL.Materials is

   function  is_Transparent (Self : in Material_type) return Boolean
   is
   begin
      return Self.diffuse (3) < 1.0;
   end;

end GL.Materials;
