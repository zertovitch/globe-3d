-------------------------------------------------------------------------
--  GLOBE_3D - GL-based, real-time, 3D engine
--
--  Copyright (c) Gautier de Montmollin/Rod Kay 2007
--  CH-8810 Horgen
--  SWITZERLAND
--  Permission granted to use this software, without any warranty,
--  for any purpose, provided this copyright note remains attached
--  and unmodified if sources are distributed further.
-------------------------------------------------------------------------

with GL,  gl.Geometry,  gl.Textures,  gl.Skins,  gl.skinned_Geometry;



package GLOBE_3D.Impostor.Simple is      -- tbd: rename 'GLOBE_3D.Impostor.standard' ?

   -- can impostor any 'visual'.


   type Impostor   is new globe_3d.impostor.Impostor with private;
   type p_Impostor is access all Impostor'Class;

   procedure pre_Calculate (o : in out Impostor);


   function  update_Required (o : access Impostor;   the_Camera   : in     globe_3d.p_Camera) return Boolean;
   procedure update          (o : in out Impostor;   the_Camera   : in     p_Camera;
                                                     texture_Pool : in     gl.textures.p_Pool);

   procedure free    (o : in out p_Impostor);




private

   type Impostor is new globe_3d.impostor.Impostor with
      record
         current_Camera_look_at_Rotation : globe_3d.Matrix_33;
      end record;

end GLOBE_3D.Impostor.Simple;
