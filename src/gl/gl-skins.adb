-------------------------------------------------------------------------
--  GL.Skins - models a 'skin' which describes the surface appearance of geometry.
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body GL.Skins is

   use GL.Geometry;
   use GL.Materials;
   use GL.Textures;

   -----------------------------------------------------------------
   -- tbd: ensure *all* skins disable *all* unneeded GL states !!!!!
   -----------------------------------------------------------------

   -- Veneers
   --

   procedure destroy (Self : in out Veneer)
   is
   begin
      null;
   end;

   procedure free (Self : in out p_Veneer)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (Veneer'Class, p_Veneer);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end;

   procedure destroy (Self : in out Skin)
   is
   begin
      null;
   end;

   procedure free (Self : in out p_Skin)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (Skin'Class, p_Skin);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end;

   -- Skin_opaque_unlit_mono_color
   --

   function  new_Veneer (Self : in Skin_opaque_unlit_mono_color;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer
   is
   pragma Unreferenced (for_Geometry, Self);
   begin
      return null;
   end;

   procedure enable (Self : in out Skin_opaque_unlit_mono_color)
   is
   begin
      GL.Disable (LIGHTING);
      GL.Disable (ALPHA_TEST);
      GL.Disable (TEXTURE_2D);
      GL.Disable (COLOR_MATERIAL);
      GL.DisableClientState (TEXTURE_COORD_ARRAY);

      Enable (BLEND); -- See 4.1.7 Blending
      BlendFunc (sfactor => SRC_ALPHA,
                 dfactor => ONE_MINUS_SRC_ALPHA);

      GL.Color (Self.Color.red,  Self.Color.green,  Self.Color.blue,  1.0);
   end;

   function  is_Transparent (Self : in     Skin_opaque_unlit_mono_color) return Boolean
   is
   pragma Unreferenced (Self);
   begin
      return False;
   end;

   -- Skin_opaque_lit_mono_color
   --

   procedure enable (Self : in out Veneer_opaque_lit_mono_color)
   is
   begin
      GL.BindBuffer        (GL.ARRAY_BUFFER, 0);    -- Disable 'vertex buffer objects'.
      GL.EnableClientState (GL.NORMAL_ARRAY);
      GL.NormalPointer     (GL_DOUBLE,  0,  to_Pointer (Self.Normals (1)(0)'Unchecked_Access));
   end;

   function  new_Veneer (Self : in     Skin_opaque_lit_mono_color;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer
   is
   pragma Unreferenced (Self);
      the_Veneer : constant p_Veneer := new Veneer_opaque_lit_mono_color' (max_Normals => vertex_Count (for_Geometry),
                                                                           normals     => vertex_Normals (for_Geometry));
   begin
      return the_Veneer;
   end;

   procedure enable (Self : in out Skin_opaque_lit_mono_color)
   is
   begin
      GL.Disable            (TEXTURE_2D);
      GL.Disable            (COLOR_MATERIAL);
      GL.DisableClientState (TEXTURE_COORD_ARRAY);
      GL.Disable            (ALPHA_TEST);

      GL.Enable    (LIGHTING);
      Set_Material (Self.Material);
   end;

   function is_Transparent (Self : in Skin_opaque_lit_mono_color) return Boolean
   is
   begin
      return is_Transparent (Self.Material);
   end;

   -- Skin: transparent unlit textured
   --

   procedure enable (Self : in out Veneer_transparent_unlit_textured)
   is
   begin
      GL.BindBuffer        (GL.ARRAY_BUFFER, 0);    -- Disable 'vertex buffer objects'.
      GL.EnableClientState (GL.TEXTURE_COORD_ARRAY);
      GL.TexCoordPointer   (2,  GL_DOUBLE,  0,  to_Pointer (Self.texture_Coordinates (1).S'Unchecked_Access));
   end;

   procedure destroy (Self : in out Skin_transparent_unlit_textured)
   is
   begin
      destroy (Self.Texture);
   end;

   function new_Veneer (Self : in Skin_transparent_unlit_textured;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer
   is
      the_Veneer : constant p_Veneer_transparent_unlit_textured
        := new Veneer_transparent_unlit_textured '(num_Coordinates     => vertex_Count (for_Geometry),
                                                   texture_coordinates => (others => (S => 0.0,  T => 0.0)));
   begin
      if Self.coordinate_Generator /= null then
         the_Veneer.texture_Coordinates := to_Coordinates (Self.coordinate_Generator.all, Vertices (for_Geometry));
      end if;

      return the_Veneer.all'Access;
   end;

   procedure enable (Self : in out Skin_transparent_unlit_textured)
   is
   begin
      GL.Disable    (LIGHTING);
      GL.Disable    (COLOR_MATERIAL);
      GL.BindBuffer (GL.ARRAY_BUFFER, 0);    -- Disable 'vertex buffer objects'.

      GL.Color     (1.0, 1.0, 1.0, 1.0);

      GL.Enable    (ALPHA_TEST);
      GL.AlphaFunc (GREATER, 0.1);

      enable (Self.Texture);
   end;

   function is_Transparent (Self : in Skin_transparent_unlit_textured) return Boolean
   is
   begin
      return is_Transparent (Self.Texture);
   end;

   -- Skin: unlit textured vbo
   --

   procedure enable (Self : in out Veneer_unlit_textured_vbo)
   is
      use GL.Buffer;
   begin
      enable (Self.texture_Coordinates);
      GL.TexCoordPointer   (2, GL_DOUBLE, 0, null);
      GL.EnableClientState (GL.TEXTURE_COORD_ARRAY);
   end;

   procedure destroy (Self : in out Skin_unlit_textured_vbo)
   is
   begin
      null;
   end;

   function new_Veneer (Self : in Skin_unlit_textured_vbo;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer
   is
      pragma Unreferenced (for_Geometry, Self);
   begin
      return new Veneer_unlit_textured_vbo;
   end;

   procedure enable (Self : in out Skin_unlit_textured_vbo)
   is
   begin
      GL.Disable (LIGHTING);
      GL.Disable (ALPHA_TEST);

      enable (Self.Texture);
   end;

   function is_Transparent (Self : in Skin_unlit_textured_vbo) return Boolean
   is
   begin
      return is_Transparent (Self.Texture);
   end;

end GL.Skins;
