

package body oGL.Frustums is


   procedure normalise (the_Planes : in out plane_Array)
   is
      use ogl.Geometry;
   begin
      for Each in the_Planes'range loop
         normalise (the_Planes (Each));
      end loop;
   end;




   function current_Planes return plane_Array
   is
      the_Planes : plane_Array;

      Proj       : array (0 .. 15) of aliased gl.Float;
      Modl       : array (0 .. 15) of aliased gl.Float;
      Clip       : array (0 .. 15) of gl.Float;

   begin

      gl.GetFloatv( GL.PROJECTION_MATRIX, proj(0)'unchecked_access );      -- Get the current PROJECTION matrix from OpenGL
      gl.GetFloatv( GL.MODELVIEW_MATRIX, modl(0)'unchecked_access );       -- Get the current MODELVIEW  matrix from OpenGL

      -- Combine the two matrices (multiply projection by modelview)
      --
      clip (0) := modl( 0) * proj( 0) + modl( 1) * proj( 4) + modl( 2) * proj( 8) + modl( 3) * proj(12);
      clip( 1) := modl( 0) * proj( 1) + modl( 1) * proj( 5) + modl( 2) * proj( 9) + modl( 3) * proj(13);
      clip( 2) := modl( 0) * proj( 2) + modl( 1) * proj( 6) + modl( 2) * proj(10) + modl( 3) * proj(14);
      clip( 3) := modl( 0) * proj( 3) + modl( 1) * proj( 7) + modl( 2) * proj(11) + modl( 3) * proj(15);

      clip( 4) := modl( 4) * proj( 0) + modl( 5) * proj( 4) + modl( 6) * proj( 8) + modl( 7) * proj(12);
      clip( 5) := modl( 4) * proj( 1) + modl( 5) * proj( 5) + modl( 6) * proj( 9) + modl( 7) * proj(13);
      clip( 6) := modl( 4) * proj( 2) + modl( 5) * proj( 6) + modl( 6) * proj(10) + modl( 7) * proj(14);
      clip( 7) := modl( 4) * proj( 3) + modl( 5) * proj( 7) + modl( 6) * proj(11) + modl( 7) * proj(15);

      clip( 8) := modl( 8) * proj( 0) + modl( 9) * proj( 4) + modl(10) * proj( 8) + modl(11) * proj(12);
      clip( 9) := modl( 8) * proj( 1) + modl( 9) * proj( 5) + modl(10) * proj( 9) + modl(11) * proj(13);
      clip(10) := modl( 8) * proj( 2) + modl( 9) * proj( 6) + modl(10) * proj(10) + modl(11) * proj(14);
      clip(11) := modl( 8) * proj( 3) + modl( 9) * proj( 7) + modl(10) * proj(11) + modl(11) * proj(15);

      clip(12) := modl(12) * proj( 0) + modl(13) * proj( 4) + modl(14) * proj( 8) + modl(15) * proj(12);
      clip(13) := modl(12) * proj( 1) + modl(13) * proj( 5) + modl(14) * proj( 9) + modl(15) * proj(13);
      clip(14) := modl(12) * proj( 2) + modl(13) * proj( 6) + modl(14) * proj(10) + modl(15) * proj(14);
      clip(15) := modl(12) * proj( 3) + modl(13) * proj( 7) + modl(14) * proj(11) + modl(15) * proj(15);


      -- Extract the RIGHT plane
      the_Planes (Right)(0) := gl.Double (clip( 3) - clip( 0));
      the_Planes (Right)(1) := gl.Double (clip( 7) - clip( 4));
      the_Planes (Right)(2) := gl.Double (clip(11) - clip( 8));
      the_Planes (Right)(3) := gl.Double (clip(15) - clip(12));

      -- Extract the LEFT plane
      the_Planes (Left)(0) := gl.Double (clip( 3) + clip( 0));
      the_Planes (Left)(1) := gl.Double (clip( 7) + clip( 4));
      the_Planes (Left)(2) := gl.Double (clip(11) + clip( 8));
      the_Planes (Left)(3) := gl.Double (clip(15) + clip(12));

      -- Extract the LOW plane
      the_Planes (Low)(0) := gl.Double (clip( 3) + clip( 1));
      the_Planes (Low)(1) := gl.Double (clip( 7) + clip( 5));
      the_Planes (Low)(2) := gl.Double (clip(11) + clip( 9));
      the_Planes (Low)(3) := gl.Double (clip(15) + clip(13));

      -- Extract the HIGH plane
      the_Planes (High)(0) := gl.Double (clip( 3) - clip( 1));
      the_Planes (High)(1) := gl.Double (clip( 7) - clip( 5));
      the_Planes (High)(2) := gl.Double (clip(11) - clip( 9));
      the_Planes (High)(3) := gl.Double (clip(15) - clip(13));

      -- Extract the FAR plane
      the_Planes (Far)(0) := gl.Double (clip( 3) - clip( 2));
      the_Planes (Far)(1) := gl.Double (clip( 7) - clip( 6));
      the_Planes (Far)(2) := gl.Double (clip(11) - clip(10));
      the_Planes (Far)(3) := gl.Double (clip(15) - clip(14));

      -- Extract the NEAR plane
      the_Planes (Near)(0) := gl.Double (clip( 3) + clip( 2));
      the_Planes (Near)(1) := gl.Double (clip( 7) + clip( 6));
      the_Planes (Near)(2) := gl.Double (clip(11) + clip(10));
      the_Planes (Near)(3) := gl.Double (clip(15) + clip(14));

      normalise (the_Planes);
      return the_Planes;
   end;


end oGL.Frustums;
