-------------------------------------------------------------------------
--  GL.Geometry
--
--  Copyright (c) Rod Kay 2016
--  AUSTRALIA
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

package body GL.Skinned_Geometry is

   use GL.Geometry, GL.Skins;

   procedure destroy (Self : in out Skinned_Geometry)
   is
   begin
      free (Self.Geometry);
      free (Self.Skin);
      free (Self.Veneer);
   end;

end GL.Skinned_Geometry;
