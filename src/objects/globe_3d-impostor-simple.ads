-------------------------------------------------------------------------
--  GLOBE_3D.Impostor.Simple
--
--  Copyright (c) Rod Kay 2007+
--  AUSTRALIA
--
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with
     GL.Textures;

package GLOBE_3D.Impostor.Simple is      -- tbd: rename 'GLOBE_3D.Impostor.standard' ?

   -- Can impostor any 'Visual'.

   type   Impostor is new GLOBE_3D.Impostor.Impostor with private;
   type p_Impostor is access all Impostor'Class;

   procedure pre_Calculate   (o : in out Impostor);

   function  update_Required (o : access Impostor;   the_Camera   : in     GLOBE_3D.p_Camera) return Boolean;
   procedure update          (o : in out Impostor;   the_Camera   : in     p_Camera;
                                                     texture_Pool : in     GL.Textures.p_Pool);
   procedure free            (o : in out p_Impostor);

private

   type Impostor is new GLOBE_3D.Impostor.Impostor with
      record
         current_Camera_look_at_Rotation : GLOBE_3D.Matrix_33;
      end record;

end GLOBE_3D.Impostor.Simple;
