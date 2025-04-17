----------------------------------------------------------------------------
--  GLOBE_3D.Impostor.terrain - An impostor specialised for terrain visuals.
--
--  Copyright (c) Rod Kay 2007+
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
----------------------------------------------------------------------------

with GLOBE_3D.Skinned_Visuals;

with
     GL.Textures;

package GLOBE_3D.Impostor.Terrain is

   -- Handles impostoring of terrain 'visuals', which have greater image precision needs, to help avoid border cracks.

   type Impostor   is new GLOBE_3D.Impostor.Impostor with private;
   type p_Impostor is access all Impostor'Class;

   overriding
   procedure pre_Calculate   (o : in out Impostor);
   overriding
   procedure set_Target      (o : in out Impostor;   Target       : in Skinned_Visuals.p_Skinned_Visual);

   overriding
   function  update_Required (o : access Impostor;   the_Camera   : in p_Camera) return Boolean;
   overriding
   procedure update          (o : in out Impostor;   the_Camera   : in p_Camera;
                                                     texture_Pool : in GL.Textures.p_Pool);
   procedure free    (o : in out p_Impostor);

private

   type Impostor is new GLOBE_3D.Impostor.Impostor with
      record
         current_Complete  : Boolean;

         prior_copy_Width  : GL.Sizei := 0;
         prior_copy_Height : GL.Sizei := 0;
         prior_Complete    : Boolean  := False;
      end record;

end GLOBE_3D.Impostor.Terrain;
