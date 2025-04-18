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

   ------------------------------------------------------------------
   --  tbd: ensure *all* skins disable *all* unneeded GL states !!!!!
   ------------------------------------------------------------------

   procedure Disable_VBO is  -- Disable 'vertex buffer objects'.
   begin
      if Disable_VBO_callback = null then
         raise Program_Error with
            "You need to define Disable_VBO_callback with an access to a procedure doing this: " &
            "GL.Extended.BindBuffer(GL.Extended.ARRAY_BUFFER, 0);";
      else
         Disable_VBO_callback.all;
      end if;
   end Disable_VBO;

   --  Veneers
   --

   procedure destroy (Self : in out Veneer)
   is
   begin
      null;
   end destroy;

   procedure free (Self : in out p_Veneer)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (Veneer'Class, p_Veneer);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end free;

   procedure destroy (Self : in out Skin)
   is
   begin
      null;
   end destroy;

   procedure free (Self : in out p_Skin)
   is
      procedure deallocate is new Ada.Unchecked_Deallocation (Skin'Class, p_Skin);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end free;

   --  Skin_opaque_unlit_mono_color
   --

   overriding
   function  new_Veneer (Self : in Skin_opaque_unlit_mono_color;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer
   is
   pragma Unreferenced (for_Geometry, Self);
   begin
      return null;
   end new_Veneer;

   overriding
   procedure enable (Self : in out Skin_opaque_unlit_mono_color)
   is
   begin
      GL.Disable (Lighting);
      GL.Disable (Alpha_Test);
      GL.Disable (Texture_2D);
      GL.Disable (Color_Material);
      GL.DisableClientState (TEXTURE_COORD_ARRAY);

      Enable (Blend); -- See 4.1.7 Blending
      BlendFunc (sfactor => SRC_ALPHA,
                 dfactor => ONE_MINUS_SRC_ALPHA);

      GL.Color (Self.Color.red,  Self.Color.green,  Self.Color.blue,  1.0);
   end enable;

   overriding
   function  is_Transparent (Self : in     Skin_opaque_unlit_mono_color) return Boolean
   is
   pragma Unreferenced (Self);
   begin
      return False;
   end is_Transparent;

   --  Skin_opaque_lit_mono_color
   --

   overriding
   procedure enable (Self : in out Veneer_opaque_lit_mono_color)
   is
   begin
      Disable_VBO;    -- Disable 'vertex buffer objects'.
      EnableClientState (NORMAL_ARRAY);
      NormalPointer     (GL_DOUBLE,  0,  to_Pointer (Self.Normals (1)(0)'Unchecked_Access));
   end enable;

   overriding
   function  new_Veneer (Self : in     Skin_opaque_lit_mono_color;   for_Geometry : in GL.Geometry.Geometry'Class) return p_Veneer
   is
   pragma Unreferenced (Self);
      the_Veneer : constant p_Veneer := new Veneer_opaque_lit_mono_color'(max_Normals => vertex_Count (for_Geometry),
                                                                          Normals     => vertex_Normals (for_Geometry));
   begin
      return the_Veneer;
   end new_Veneer;

   overriding
   procedure enable (Self : in out Skin_opaque_lit_mono_color)
   is
   begin
      GL.Disable            (Texture_2D);
      GL.Disable            (Color_Material);
      GL.DisableClientState (TEXTURE_COORD_ARRAY);
      GL.Disable            (Alpha_Test);

      GL.Enable    (Lighting);
      Set_Material (Self.Material);
   end enable;

   overriding
   function is_Transparent (Self : in Skin_opaque_lit_mono_color) return Boolean
   is
   begin
      return Is_transparent (Self.Material);
   end is_Transparent;

   --  Skin: transparent unlit textured
   --

   overriding
   procedure enable (Self : in out Veneer_transparent_unlit_textured)
   is
   begin
      Disable_VBO;    -- Disable 'vertex buffer objects'.
      EnableClientState (GL.TEXTURE_COORD_ARRAY);
      GL.TexCoordPointer   (2,  GL_DOUBLE,  0,  to_Pointer (Self.texture_Coordinates (1).S'Unchecked_Access));
   end enable;

   overriding
   procedure destroy (Self : in out Skin_transparent_unlit_textured)
   is
   begin
      destroy (Self.Texture);
   end destroy;

   overriding
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
   end new_Veneer;

   overriding
   procedure enable (Self : in out Skin_transparent_unlit_textured)
   is
   begin
      GL.Disable    (Lighting);
      GL.Disable    (Color_Material);
      Disable_VBO;    -- Disable 'vertex buffer objects'.

      GL.Color     (1.0, 1.0, 1.0, 1.0);

      GL.Enable    (Alpha_Test);
      GL.AlphaFunc (GREATER, 0.1);

      enable (Self.Texture);
   end enable;

   overriding
   function is_Transparent (Self : in Skin_transparent_unlit_textured) return Boolean
   is
   begin
      return is_Transparent (Self.Texture);
   end is_Transparent;

end GL.Skins;
