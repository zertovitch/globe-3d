-------------------------------------------------------------------------
--  GL.Skinned_Geometry - an association of a skin with a geometry primitive.
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL.Geometry,
     GL.Skins;

package GL.Skinned_Geometry is

   type Skinned_Geometry is
      record
         Geometry : GL.Geometry.p_Geometry;
         Skin     : GL.Skins.p_Skin;
         Veneer   : GL.Skins.p_Veneer;
      end record;

   type Skinned_Geometries is array (Positive range <>) of Skinned_Geometry;

   null_skinned_geometries : constant Skinned_Geometries (1 .. 0) := (others => (Geometry => null,
                                                                                 Skin     => null,
                                                                                 Veneer   => null));

   procedure destroy (Self : in out Skinned_Geometry);

end GL.Skinned_Geometry;
