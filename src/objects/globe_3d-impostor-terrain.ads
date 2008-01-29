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



package GLOBE_3D.Impostor.Terrain is

   -- handles impostoring of terrain 'visuals', which has greater image precision needs, to help avoid border cracks.


   type Impostor   is new globe_3d.impostor.Impostor with private;
   type p_Impostor is access all Impostor'Class;


   procedure pre_Calculate     (o : in out Impostor);
   procedure set_Target        (o : in out Impostor;   Target : in p_Visual);


   function  update_Required (o : access Impostor;   the_Camera   : in     globe_3d.p_Camera) return Boolean;
   procedure update          (o : in out Impostor;   the_Camera   : in     p_Camera;
                                                     texture_Pool : in     gl.textures.p_Pool);


   procedure free    (o : in out p_Impostor);





private


   type Impostor is new globe_3d.impostor.Impostor with
      record
         current_Complete    : Boolean;

         prior_copy_Width    : gl.SizeI := 0;
         prior_copy_Height   : gl.SizeI := 0;
         prior_Complete      : Boolean  := False;
      end record;


end GLOBE_3D.Impostor.Terrain;
