-------------------------------------------------------------------------
--  GL.Frustum
--
--  Copyright (c) Gautier de Montmollin/Rod Kay 2001..2007
--  CH-8810 Horgen
--  SWITZERLAND
--
--  Permission granted to:
--  1/ use this library, without any warranty, for any purpose;
--  2/ modify this library's sources (specification, body of this
--     package and of child packages on which it depends) in any
--     way, with an appropriate commenting of changes;
--  3/ copy and distribute this library's sources without restriction,
--     provided this copyright note remains attached and unmodified.
-------------------------------------------------------------------------

with GL.Geometry;


package GL.Frustums is


   type plane_Id is (Left, Right, High, Low, Near, Far);

   type plane_Array is array (plane_Id) of gl.geometry.Plane;

   procedure normalise (the_Planes : in out plane_Array);



   function current_Planes return plane_Array;
   --
   -- returns the frustum planes calculated from the current GL projection and modelview matrices.



end GL.Frustums;
