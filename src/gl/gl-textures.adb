-------------------------------------------------------------------------
--  GL.Textures - GL Textures model
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL.IO,
     GL.Errors,
     Ada.Characters.Handling,
     Ada.Text_IO;

use Ada.Text_IO;

package body GL.Textures is

   -- Names

   function new_texture_Name return texture_Name
   is
      the_Name : aliased texture_Name;
   begin
      GL.GenTextures (1,  the_Name'Unchecked_Access);
      return the_Name;
   end;

   procedure free (the_texture_Name : in texture_Name)
   is
      the_Name : aliased texture_Name := the_texture_Name;
   begin
      GL.DeleteTextures (1, the_Name'Unchecked_Access);
   end;

   -- Coordinates
   --

   function to_texture_Coordinates_xz (the_Points  : in GL.Geometry.Vertex_array;
                                       Transform_S : in texture_Transform;          -- Transforms point X ordinate.
                                       Transform_T : in texture_Transform)          -- Transforms point Z ordinate.
                                       return p_Coordinate_2D_array
   is
      the_Coords : constant p_Coordinate_2D_array := new Coordinate_2D_array (1 .. the_Points'Last);
   begin
      for Each in the_Points'Range loop
         declare
            the_Vertex : GL.Geometry.Vertex renames the_Points (Each);
         begin
            the_Coords (Each).S :=       (the_Vertex (0) + Transform_S.Offset) * Transform_S.Scale;
            the_Coords (Each).T := 1.0 - (the_Vertex (2) + Transform_T.Offset) * Transform_T.Scale;
         end;
      end loop;

      return the_Coords;
   end;

   function to_texture_Coordinates_xz (the_Points  : in GL.Geometry.Vertex_array;
                                       Transform_S : in texture_Transform;          -- Transforms point X ordinate.
                                       Transform_T : in texture_Transform)          -- Transforms point Z ordinate.
                                       return Coordinate_2D_array
   is
      the_Coords : Coordinate_2D_array (1 .. the_Points'Last);
   begin
      for Each in the_Points'Range loop
         declare
            the_Vertex : GL.Geometry.Vertex renames the_Points (Each);
         begin
            the_Coords (Each).S :=         (the_Vertex (0) + Transform_S.Offset) * Transform_S.Scale;
            the_Coords (Each).T := 1.0  -  (the_Vertex (2) + Transform_T.Offset) * Transform_T.Scale;
         end;
      end loop;

      return the_Coords;
   end;

   -- xz_Generator

   function to_Coordinates (Self : in xz_Generator;   the_Vertices : in GL.Geometry.Vertex_array) return GL.Textures.p_Coordinate_2D_array
   is
   begin
      return to_texture_Coordinates_xz (the_Vertices, Self.Transform_S, Self.Transform_T);
   end;

   function to_Coordinates (Self : in xz_Generator;   the_Vertices : in GL.Geometry.Vertex_array) return GL.Textures.Coordinate_2D_array
   is
   begin
      return to_texture_Coordinates_xz (the_Vertices, Self.Transform_S, Self.Transform_T);
   end;

   -- texture objects

   function new_Texture (image_Filename : in String) return Object
   is
      use Ada.Characters.Handling;

      Extension : constant String := image_Filename (image_Filename'Last - 2 .. image_Filename'Last);

      the_Texture : Object;
   begin
      the_Texture.Name := new_texture_Name;

      if To_Lower (Extension) = "bmp" then
         GL.IO.Load (image_Filename,  GL.IO.BMP,  Integer (the_Texture.Name),  blending_hint => the_Texture.is_Transparent);

      elsif To_Lower (Extension) = "tga" then
         GL.IO.Load (image_Filename,  GL.IO.TGA,  Integer (the_Texture.Name),  blending_hint => the_Texture.is_Transparent);
      else
         raise unsupported_format_Error;
      end if;

      -- tbd: if not found, look in 'global' and 'level' zip files also, ala gautiers 'globe_3d.textures'.
      return the_Texture;
   end;

   procedure destroy (Self : in out Object)
   is
   begin
      if Self.Pool = null then
         free (Self.Name);
      else
         free (Self.Pool.all, Self);
      end if;
   end;

   procedure set_Name (Self : in out Object;   To : in GL.Uint)
   is
   begin
      Self.Name := To;
   end;

   function Name (Self : in Object) return GL.Uint
   is
   begin
      return Self.Name;
   end;

   function  is_Transparent (Self : in Object) return Boolean
   is
   begin
      return Self.is_Transparent;
   end;

   procedure enable (Self : in out Object)
   is
   begin
      pragma Assert (Self.Name > 0);

      GL.Enable      (GL.TEXTURE_2D);
      GL.BindTexture (GL.TEXTURE_2D, Self.Name);
   end;

   -- Pool
   --

   null_Image : array (1 .. 10_000_000) of aliased GL.Ubyte := (others => 0);

   -- tbd: add texture properties as 'in' parameters to habdle different types of textures.
   --
   function new_Texture (From : access Pool;   min_Width  : in Positive;
                                               min_Height : in Positive) return Object
   is
      the_Texture : aliased Object;

      Size_width  : constant Size := to_Size (min_Width);
      Size_height : constant Size := to_Size (min_Height);

      unused_texture_List : p_pool_texture_List := From.unused_Textures_for_size (Size_width, Size_height);
   begin
      if unused_texture_List = null then
         unused_texture_List                                     := new pool_texture_List;
         From.unused_Textures_for_size (Size_width, Size_height) := unused_texture_List;
      end if;

      -- Search for existing, but unused, object.
      --
      if unused_texture_List.Last > 0 then -- An existing unused texture has been found.
         the_Texture              := unused_texture_List.Textures (unused_texture_List.Last);
         unused_texture_List.Last := unused_texture_List.Last - 1;

         enable (the_Texture);

         GL.TexImage2D  (GL.TEXTURE_2D,  0,  GL.RGBA,
                         power_of_2_Ceiling (min_Width), power_of_2_Ceiling (min_Height),
                         0,
                         --gl.RGBA, gl.GL_UNSIGNED_BYTE, null);    -- nb: actual image is not initialised.
                         GL.RGBA, GL.GL_UNSIGNED_BYTE, null_Image (null_Image'First)'Access);    -- NB: Actual image is not initialised.
      else
         -- No existing, unused texture found, so create a new one.
         --
         the_Texture.Width  := Size_width;
         the_Texture.Height := Size_height;

         the_Texture.Pool := From.all'Access;

         the_Texture.Name := new_texture_Name;
         enable (the_Texture);

         PixelStore ( UNPACK_ALIGNMENT, 1 );                        -- tbd: these properties are tailored for impostors
         --TexParameter ( TEXTURE_2D, TEXTURE_WRAP_S, REPEAT );       --      make them user settable !
         --TexParameter ( TEXTURE_2D, TEXTURE_WRAP_T, REPEAT );
          -- TexParameter ( TEXTURE_2D, TEXTURE_WRAP_S, CLAMP );       --      make them user settable !
          -- TexParameter ( TEXTURE_2D, TEXTURE_WRAP_T, CLAMP );
         TexParameter ( TEXTURE_2D, TEXTURE_WRAP_S, CLAMP_TO_EDGE );       --      make them user settable !
         TexParameter ( TEXTURE_2D, TEXTURE_WRAP_T, CLAMP_TO_EDGE );

         --TexParameter (TEXTURE_2D, TEXTURE_MAG_FILTER, NEAREST);
         --TexParameter (TEXTURE_2D, TEXTURE_MIN_FILTER, NEAREST);
         TexParameter ( TEXTURE_2D, TEXTURE_MAG_FILTER, LINEAR);
         TexParameter ( TEXTURE_2D, TEXTURE_MIN_FILTER, LINEAR);

         TexEnv ( TEXTURE_ENV, TEXTURE_ENV_MODE, MODULATE );
         --TexEnv ( TEXTURE_ENV, TEXTURE_ENV_MODE, DECAL );

         GL.TexImage2D (GL.TEXTURE_2D,  0,  GL.RGBA,
                        power_of_2_Ceiling (min_Width), power_of_2_Ceiling (min_Height),
                        0,
                        --gl.RGBA, gl.GL_UNSIGNED_BYTE, null);    -- nb: actual image is not initialised.
                        GL.RGBA, GL.GL_UNSIGNED_BYTE, null_Image (null_Image'First)'Access);    -- NB: Actual image is not initialised.

         GL.Errors.Log;
      end if;

      return the_Texture;
   end;

   procedure free (Self : in out Pool;   the_Texture : in Object)
   is
   begin
      if the_Texture.Name = 0 then
         return;
      end if;

      declare
         unused_texture_List : constant p_pool_texture_List := Self.unused_Textures_for_size (the_Texture.Width, the_Texture.Height);
      begin
         unused_texture_List.Last                                := unused_texture_List.Last + 1;
         unused_texture_List.Textures (unused_texture_List.Last) := the_Texture;
      end;
   end;

   procedure vacuum (Self : in out Pool)
   is
   begin
      for each_Width in Self.unused_Textures_for_size'Range (1)
      loop
         for each_Height in Self.unused_Textures_for_size'Range (2)
         loop
            declare
               unused_texture_List : constant p_pool_texture_List := Self.unused_Textures_for_size (each_Width, each_Height);
            begin
               if unused_texture_List /= null
               then
                  for Each in 1 .. unused_texture_List.Last
                  loop
                     free (unused_texture_List.Textures (Each).Name);
                  end loop;

                  unused_texture_List.Last := 0;
               end if;
            end;
         end loop;
      end loop;
   end;

   function to_Size (From : in Positive) return Size
   is
   begin
      if    From <= 2    then  return s2;
      elsif From <= 4    then  return s4;
      elsif From <= 8    then  return s8;
      elsif From <= 16   then  return s16;
      elsif From <= 32   then  return s32;
      elsif From <= 64   then  return s64;
      elsif From <= 128  then  return s128;
      elsif From <= 256  then  return s256;
      elsif From <= 512  then  return s512;
      elsif From <= 1024 then  return s1024;
      elsif From <= 2048 then  return s2048;
      end if;

      Put_Line ("to_Size: From: " & Positive'Image (From));

      raise Constraint_Error;
   end;

   function power_of_2_Ceiling (From : in Positive) return GL.Sizei
   is
   begin
      if    From <= 2    then  return 2;
      elsif From <= 4    then  return 4;
      elsif From <= 8    then  return 8;
      elsif From <= 16   then  return 16;
      elsif From <= 32   then  return 32;
      elsif From <= 64   then  return 64;
      elsif From <= 128  then  return 128;
      elsif From <= 256  then  return 256;
      elsif From <= 512  then  return 512;
      elsif From <= 1024 then  return 1024;
      elsif From <= 2048 then  return 2048;
      end if;

      raise Constraint_Error;
   end;

   function Size_width  (Self : in Object) return Size
   is
   begin
      return Self.Width;
   end;

   function Size_height (Self : in Object) return Size
   is
   begin
      return Self.Height;
   end;

end GL.Textures;
