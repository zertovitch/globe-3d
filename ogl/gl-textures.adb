-------------------------------------------------------------------------
--  GL.Textures - GL Textures model
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with gl.IO;
with gl.Errors;

--with Ada.Directories;
with ada.Characters.handling;
with Ada.Text_IO; use Ada.Text_IO;



package body gl.Textures is


   -- names

   function new_texture_Name return texture_Name
   is
      the_Name : aliased texture_Name;
   begin
      gl.genTextures (1,  the_Name'unchecked_access);
      return the_Name;
   end;



   procedure free (the_texture_Name : in texture_Name)
   is
      the_Name : aliased texture_Name := the_texture_Name;
   begin
      gl.deleteTextures (1, the_Name'unchecked_access);
   end;




   -- coordinates
   --

   function to_texture_Coordinates_xz (the_Points  : in gl.geometry.vertex_Array;
                                       Transform_S : in texture_Transform;          -- transforms point X ordinate.
                                       Transform_T : in texture_Transform)          -- transforms point Z ordinate.
                                       return p_Coordinate_2D_array
   is
      the_Coords : constant p_Coordinate_2D_array := new Coordinate_2D_array (1 .. the_Points'Last);
   begin
      for Each in the_Points'range loop
         declare
            the_Vertex : gl.geometry.Vertex renames the_Points (each);
         begin
            the_Coords (each).S :=         (the_Vertex (0) + Transform_s.Offset) * Transform_s.Scale;
            the_Coords (each).T := 1.0  -  (the_Vertex (2) + Transform_t.Offset) * Transform_t.Scale;
         end;
      end loop;

      return the_Coords;
   end;




   function to_texture_Coordinates_xz (the_Points  : in gl.geometry.vertex_Array;
                                       Transform_S : in texture_Transform;          -- transforms point X ordinate.
                                       Transform_T : in texture_Transform)          -- transforms point Z ordinate.
                                       return Coordinate_2D_array
   is
      the_Coords : Coordinate_2D_array (1 .. the_Points'Last);
   begin
      for Each in the_Points'range loop
         declare
            the_Vertex : gl.geometry.Vertex renames the_Points (each);
         begin
            the_Coords (each).S :=         (the_Vertex (0) + Transform_s.Offset) * Transform_s.Scale;
            the_Coords (each).T := 1.0  -  (the_Vertex (2) + Transform_t.Offset) * Transform_t.Scale;
         end;
      end loop;

      return the_Coords;
   end;




   -- xz_Generator

   function to_Coordinates (Self : in xz_Generator;   the_Vertices : in gl.geometry.Vertex_array) return gl.textures.p_Coordinate_2D_array
   is
   begin
      return to_texture_Coordinates_xz (the_Vertices, self.transform_S, self.transform_T);
   end;


   function to_Coordinates (Self : in xz_Generator;   the_Vertices : in gl.geometry.Vertex_array) return gl.textures.Coordinate_2D_array
   is
   begin
      return to_texture_Coordinates_xz (the_Vertices, self.transform_S, self.transform_T);
   end;






   -- texture objects

   function new_Texture (image_Filename : in String) return Object
   is
      use ada.Characters.handling;

      Extension : constant String := image_Filename (image_Filename'Last - 2 .. image_Filename'Last);

      the_Texture : Object;
   begin
      the_Texture.Name := new_texture_Name;

      if to_Lower (Extension) = "bmp" then
         gl.io.load (image_Filename,  gl.io.BMP,  Integer (the_Texture.Name),  blending_hint => the_Texture.is_Transparent);

      elsif to_Lower (Extension) = "tga" then
         gl.io.load (image_Filename,  gl.io.TGA,  Integer (the_Texture.Name),  blending_hint => the_Texture.is_Transparent);
      else
         raise unsupported_format_Error;
      end if;

      -- tbd: if not found, look in 'global' and 'level' zip files also, ala gautiers 'globe_3d.textures'.
      return the_Texture;
   end;






   procedure destroy (Self : in out Object)
   is
   begin
      if self.Pool = null then
         free (self.Name);
      else
         free (self.Pool.all, Self);
      end if;
   end;




   procedure set_Name (Self : in out Object;   To : in gl.uInt)
   is
   begin
      self.Name := To;
   end;



   function Name (Self : in     Object) return gl.uInt
   is
   begin
      return self.Name;
   end;




   function  is_Transparent (Self : in     Object) return Boolean
   is
   begin
      return self.is_Transparent;
   end;




   procedure enable (Self : in out Object)
   is
   begin
      pragma Assert (self.Name > 0);

      gl.enable      (gl.TEXTURE_2D);
      gl.bindTexture (gl.TEXTURE_2D, self.Name);
   end;







   -- Pool
   --

   null_Image : array (1 .. 10_000_000) of aliased gl.uByte := (others => 0);


   -- tbd: add texture properties as 'in' parameters to habdle different types of textures.
   --
   function new_Texture (From : access Pool;   min_Width  : in Positive;
                                               min_Height : in Positive) return Object
   is
      the_Texture : aliased Object;

      Size_width  : constant Size := to_Size (min_Width);
      Size_height : constant Size := to_Size (min_Height);

      unused_texture_List : p_pool_texture_List := from.unused_Textures_for_size (Size_width, Size_height);
   begin
      if unused_texture_List = null then
         unused_texture_List                                     := new pool_texture_List;
         from.unused_Textures_for_size (Size_width, Size_height) := unused_texture_List;
      end if;

      -- search for existing, but unused, object.
      --
      if unused_texture_List.Last > 0 then -- an existing unused texture has been found
         the_Texture              := unused_texture_List.Textures (unused_texture_List.Last);
         unused_texture_List.Last := unused_texture_List.Last - 1;

         enable (the_Texture);

         gl.texImage2D  (gl.TEXTURE_2D,  0,  gl.RGBA,
                         power_of_2_Ceiling (min_Width), power_of_2_Ceiling (min_Height),
                         0,
                         --gl.RGBA, gl.GL_UNSIGNED_BYTE, null);    -- nb: actual image is not initialised.
                         gl.RGBA, gl.GL_UNSIGNED_BYTE, null_Image (null_image'First)'access);    -- nb: actual image is not initialised.
      else
         -- no existing, unused texture found, so create a new one.
         --
         the_Texture.Width  := Size_width;
         the_Texture.Height := Size_height;

         the_Texture.Pool := From.all'access;


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


         gl.texImage2D  (gl.TEXTURE_2D,  0,  gl.RGBA,
                         power_of_2_Ceiling (min_Width), power_of_2_Ceiling (min_Height),
                         0,
                         --gl.RGBA, gl.GL_UNSIGNED_BYTE, null);    -- nb: actual image is not initialised.
                         gl.RGBA, gl.GL_UNSIGNED_BYTE, null_Image (null_image'First)'access);    -- nb: actual image is not initialised.

         gl.Errors.log;  -- tbd: only for debug.
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

      for each_Width in self.unused_Textures_for_size'range (1) loop
         for each_Height in self.unused_Textures_for_size'range (2) loop
            declare
               unused_texture_List : constant p_pool_texture_List := Self.unused_Textures_for_size (each_Width, each_Height);
            begin
               if unused_texture_List /= null then

                  for Each in 1 .. unused_texture_List.Last loop
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

      put_line ("to_Size: From: " & Positive'image (From));

      raise Constraint_Error;
   end;


   function power_of_2_Ceiling (From : in Positive) return gl.Sizei
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
      return self.Width;
   end;



   function Size_height (Self : in Object) return Size
   is
   begin
      return self.Height;
   end;




end gl.Textures;
