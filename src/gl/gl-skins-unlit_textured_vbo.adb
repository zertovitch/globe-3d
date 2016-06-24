-------------------------------------------------------------------------
--  GL.Skins - models a 'skin' which describes the surface appearance of geometry.
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

package body GL.Skins.unlit_textured_vbo is

   use GL.Geometry;
   use GL.Materials;
   use GL.Textures;

   procedure enable (Self : in out Veneer)
   is
      use GL.Buffer;
   begin
      enable (Self.texture_Coordinates);
      GL.TexCoordPointer   (2, GL_DOUBLE, 0, null);
      GL.EnableClientState (GL.TEXTURE_COORD_ARRAY);
   end;

   procedure destroy (Self : in out Skin)
   is
   begin
      null;
   end;

   function new_Veneer (Self : in     Skin;   for_Geometry : in GL.Geometry.Geometry'Class) return GL.Skins.p_Veneer
   is
      pragma Unreferenced (for_Geometry, Self);
   begin
      return new Veneer;
   end;

   procedure enable (Self : in out Skin)
   is
   begin
      GL.Disable (LIGHTING);
      GL.Disable (ALPHA_TEST);

      enable (Self.Texture);
   end;

   function is_Transparent (Self : in Skin) return Boolean
   is
   begin
      return is_Transparent (Self.Texture);
   end;

end GL.Skins.unlit_textured_vbo;
