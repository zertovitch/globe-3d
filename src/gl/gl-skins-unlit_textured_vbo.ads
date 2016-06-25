-------------------------------------------------------------------------
--  GL.Skins.unlit_textured_vbo
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with
     GL.Buffer.Texture_coords;

package GL.Skins.unlit_textured_vbo is

   type Veneer is new GL.Skins.Veneer with
      record
         texture_Coordinates : GL.Buffer.Texture_coords.Object;
      end record;

   type p_Veneer is access all Veneer'Class;

   overriding
   procedure enable (Self : in out Veneer);


   type Skin is new GL.Skins.Skin with
      record
         Texture : GL.Textures.Object;
      end record;

   type p_Skin is access all Skin;

   overriding
   procedure destroy (Self : in out Skin);

   overriding
   function  new_Veneer (Self : in     Skin;   for_Geometry : in GL.Geometry.Geometry'Class) return GL.Skins.p_Veneer;
   overriding
   procedure enable     (Self : in out Skin);

   overriding
   function  is_Transparent (Self : in     Skin) return Boolean;

end GL.Skins.unlit_textured_vbo;
