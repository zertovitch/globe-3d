-------------------------------------------------------------------------
--  GLOBE_3D.Sprite
--
--  Copyright (c) Rod Kay 2007+
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with
     GL.Geometry,
     GL.Skins,
     GL.Skinned_Geometry;

package GLOBE_3D.Sprite is

   -- A 'Visual' which is composed of several 'gl.skinned_Geometry' items.

   type Sprite (max_Geometries : Positive) is new Visual with
      record
         skinned_Geometries     : GL.Skinned_Geometry.Skinned_Geometries (1 .. max_Geometries);
         skinned_geometry_Count : Natural := 0;

         -- private:
         is_Transparent : Boolean := False;
         face_Count     : Natural := 0;
         Bounds         : GL.Geometry.Bounds_record;
      end record;

   type p_Sprite      is access all Sprite'Class;
   type p_sprite_Grid is array (Positive range <>, Positive range <>) of p_Sprite;

   overriding
   procedure destroy        (o : in out Sprite);

   overriding
   procedure set_Alpha      (o : in out Sprite;   Alpha : in GL.Double);
   overriding
   function  is_Transparent (o : in     Sprite) return Boolean;

   overriding
   function skinned_Geometries (o : in Sprite) return GL.Skinned_Geometry.Skinned_Geometries;

   procedure add            (o : in out Sprite;   Geometry : access GL.Geometry.Geometry'Class;
                                                  Skin     : access GL.Skins.Skin'Class);

   overriding
   procedure Pre_calculate  (o : in out Sprite);

   overriding
   procedure Display        (o : in out Sprite;   clip : in Clipping_data);

   overriding
   function face_Count      (o : in Sprite) return Natural;
   overriding
   function Bounds          (o : in Sprite) return GL.Geometry.Bounds_record;

end GLOBE_3D.Sprite;
