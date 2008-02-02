-------------------------------------------------------------------------
--  GL.Skins - models a 'skin' which describes the surface appearance of geometry.
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with Ada.Text_IO;
with ada.unchecked_Deallocation;



package body gl.Skins is


   use gl.Geometry;
   use gl.Textures;


   -----------------------------------------------------------------
   -- tbd: ensure *all* skins disable *all* unneeded GL states !!!!!
   -----------------------------------------------------------------



   -- materials
   --


   function  is_Transparent (Self : in     Material_type) return Boolean
   is
   begin
      return Self.diffuse (3) < 1.0;
   end;






   -- veneers
   --

   procedure destroy (Self : in out Veneer)
   is
   begin
      null;
   end;




   procedure free (Self : in out p_Veneer)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Veneer'Class, p_Veneer);
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
      procedure deallocate is new ada.unchecked_Deallocation (Skin'Class, p_Skin);
   begin
      destroy    (Self.all);
      deallocate (Self);
   end;






   procedure Set_Material (m: Material_type) is
      use GL;
   begin
      Material (FRONT_AND_BACK, AMBIENT,   m.ambient);
      Material (FRONT_AND_BACK, DIFFUSE,   m.diffuse);
      Material (FRONT_AND_BACK, SPECULAR,  m.specular);
      Material (FRONT_AND_BACK, EMISSION,  m.emission);
      Material (FRONT_AND_BACK, SHININESS, m.shininess);
   end Set_Material;




   -- Skin_opaque_unlit_mono_color
   --

   function  new_Veneer (Self : in     Skin_opaque_unlit_mono_color;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer
   is
   begin
      return null;
   end;



   procedure enable (Self : in out Skin_opaque_unlit_mono_color)
   is
   begin
      gl.Disable (LIGHTING);
      gl.disable (ALPHA_TEST);

      Enable (BLEND); -- See 4.1.7 Blending
      BlendFunc (sfactor => SRC_ALPHA,
                 dfactor => ONE_MINUS_SRC_ALPHA);

      gl.Color   (self.Color.Red,  self.Color.Green,  self.Color.Blue,  1.0);
   end;


   function  is_Transparent (Self : in     Skin_opaque_unlit_mono_color) return Boolean
   is
   begin
      return False;
   end;





   -- Skin_opaque_lit_mono_color
   --

   procedure enable (Self : in out Veneer_opaque_lit_mono_color)
   is
   begin
      gl.bindBuffer        (gl.ARRAY_BUFFER, 0);    -- disable 'vertex buffer objects'
      gl.enableClientState (gl.NORMAL_ARRAY);
      gl.normalPointer     (GL_DOUBLE,  0,  to_Pointer (self.Normals (1)(0)'unchecked_access));
   end;





   function  new_Veneer (Self : in     Skin_opaque_lit_mono_color;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer
   is
      the_Veneer : constant p_Veneer := new Veneer_opaque_lit_mono_color' (max_normals => vertex_Count (for_Geometry),
                                                                           normals     => vertex_Normals (for_Geometry));
   begin
      return the_Veneer;
   end;





   procedure enable (Self : in out Skin_opaque_lit_mono_color)
   is
   begin
      gl.disable            (TEXTURE_2D);
      gl.disable            (COLOR_MATERIAL);
      gl.disableClientState (TEXTURE_COORD_ARRAY);
      gl.disable            (ALPHA_TEST);

      gl.enable    (LIGHTING);
      set_Material (self.Material);
   end;




   function  is_Transparent (Self : in     Skin_opaque_lit_mono_color) return Boolean
   is
   begin
      return is_Transparent (self.Material);
   end;




   -- Skin: transparent unlit textured
   --


   procedure enable (Self : in out Veneer_transparent_unlit_textured)
   is
   begin
      gl.bindBuffer        (gl.ARRAY_BUFFER, 0);    -- disable 'vertex buffer objects'
      gl.enableClientState (gl.TEXTURE_COORD_ARRAY);
      gl.texCoordPointer   (2,  GL_DOUBLE,  0,  to_Pointer (self.texture_Coordinates (1).S'unchecked_access));
   end;




   procedure destroy (Self : in out Skin_transparent_unlit_textured)
   is
   begin
      destroy (self.Texture);
   end;




   function new_Veneer (Self : in     Skin_transparent_unlit_textured;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer
   is
      the_Veneer : constant p_Veneer_transparent_unlit_textured
        :=   new Veneer_transparent_unlit_textured '(num_coordinates     => vertex_Count (for_Geometry),
                                                     texture_coordinates => (others => (S => 0.0,  T => 0.0)));
   begin
      if self.coordinate_generator /= null then
         the_veneer.texture_coordinates := to_Coordinates (self.coordinate_generator.all, Vertices (for_Geometry));
      end if;

      return the_Veneer.all'access;
   end;




   procedure enable (Self : in out Skin_transparent_unlit_textured)
   is
   begin
      gl.disable    (LIGHTING);
      gl.disable    (COLOR_MATERIAL);
      gl.bindBuffer (gl.ARRAY_BUFFER, 0);    -- disable 'vertex buffer objects'

      gl.color     (1.0, 1.0, 1.0, 1.0);

      gl.enable    (ALPHA_TEST);
      gl.alphaFunc (GREATER, 0.1);

      enable (self.Texture);
   end;


   function  is_Transparent (Self : in     Skin_transparent_unlit_textured) return Boolean
   is
   begin
      return is_Transparent (self.Texture);
   end;




   -- Skin: unlit textured vbo
   --

   procedure enable (Self : in out Veneer_unlit_textured_vbo)
   is
      use gl.Buffer;
   begin
      enable (self.texture_Coordinates);
      gl.texCoordPointer   (2, GL_DOUBLE, 0, null);
      gl.enableClientState (gl.TEXTURE_COORD_ARRAY);
   end;



   procedure destroy (Self : in out Skin_unlit_textured_vbo)
   is
   begin
      null;
   end;



   function  new_Veneer (Self : in     Skin_unlit_textured_vbo;   for_Geometry : in gl.Geometry.Geometry'Class) return p_Veneer
   is
   begin
      return new Veneer_unlit_textured_vbo;
   end;



   procedure enable (Self : in out Skin_unlit_textured_vbo)
   is
   begin
      gl.disable (LIGHTING);
      gl.disable (ALPHA_TEST);

      enable (self.Texture);
   end;



   function  is_Transparent (Self : in     Skin_unlit_textured_vbo) return Boolean
   is
   begin
      return is_Transparent (self.Texture);
   end;



end gl.Skins;
