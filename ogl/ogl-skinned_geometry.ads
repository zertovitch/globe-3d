-------------------------------------------------------------------------
--  GL.skinned_Geometry - an association of a skin with a geometry primitive.
--
--  Copyright (c) Rod Kay 2007
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with oGL.Geometry;
with oGL.Skins;



package ogl.skinned_Geometry is



   type skinned_Geometry is
      record
         Geometry : ogl.Geometry.p_Geometry;
         Skin     : ogl.Skins.p_Skin;
         Veneer   : ogl.Skins.p_Veneer;
      end record;


   type skinned_Geometrys is array (Positive range <>) of skinned_Geometry;


   null_skinned_Geometrys : constant skinned_Geometrys (1 .. 0) := (others => (Geometry => null,
                                                                               Skin     => null,
                                                                               Veneer   => null));

--   function Bounds (Self : in

   procedure destroy (Self : in out skinned_Geometry);




end ogl.skinned_Geometry;
