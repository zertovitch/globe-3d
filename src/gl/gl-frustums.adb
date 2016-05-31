
package body GL.Frustums is

   procedure normalise (the_Planes : in out plane_Array)
   is
      use GL.Geometry;
   begin
      for Each in the_Planes'Range loop
         normalise (the_Planes (Each));
      end loop;
   end;

   function current_Planes return plane_Array
   is
      the_Planes : plane_Array;

      Proj       : array (0 .. 15) of aliased GL.Float;
      Modl       : array (0 .. 15) of aliased GL.Float;
      Clip       : array (0 .. 15) of GL.Float;

   begin

      GL.GetFloatv( GL.PROJECTION_MATRIX, Proj(0)'Unchecked_Access );      -- Get the current PROJECTION matrix from OpenGL
      GL.GetFloatv( GL.MODELVIEW_MATRIX, Modl(0)'Unchecked_Access );       -- Get the current MODELVIEW  matrix from OpenGL

      -- Combine the two matrices (multiply projection by modelview)
      --
      Clip (0) := Modl( 0) * Proj( 0) + Modl( 1) * Proj( 4) + Modl( 2) * Proj( 8) + Modl( 3) * Proj(12);
      Clip( 1) := Modl( 0) * Proj( 1) + Modl( 1) * Proj( 5) + Modl( 2) * Proj( 9) + Modl( 3) * Proj(13);
      Clip( 2) := Modl( 0) * Proj( 2) + Modl( 1) * Proj( 6) + Modl( 2) * Proj(10) + Modl( 3) * Proj(14);
      Clip( 3) := Modl( 0) * Proj( 3) + Modl( 1) * Proj( 7) + Modl( 2) * Proj(11) + Modl( 3) * Proj(15);

      Clip( 4) := Modl( 4) * Proj( 0) + Modl( 5) * Proj( 4) + Modl( 6) * Proj( 8) + Modl( 7) * Proj(12);
      Clip( 5) := Modl( 4) * Proj( 1) + Modl( 5) * Proj( 5) + Modl( 6) * Proj( 9) + Modl( 7) * Proj(13);
      Clip( 6) := Modl( 4) * Proj( 2) + Modl( 5) * Proj( 6) + Modl( 6) * Proj(10) + Modl( 7) * Proj(14);
      Clip( 7) := Modl( 4) * Proj( 3) + Modl( 5) * Proj( 7) + Modl( 6) * Proj(11) + Modl( 7) * Proj(15);

      Clip( 8) := Modl( 8) * Proj( 0) + Modl( 9) * Proj( 4) + Modl(10) * Proj( 8) + Modl(11) * Proj(12);
      Clip( 9) := Modl( 8) * Proj( 1) + Modl( 9) * Proj( 5) + Modl(10) * Proj( 9) + Modl(11) * Proj(13);
      Clip(10) := Modl( 8) * Proj( 2) + Modl( 9) * Proj( 6) + Modl(10) * Proj(10) + Modl(11) * Proj(14);
      Clip(11) := Modl( 8) * Proj( 3) + Modl( 9) * Proj( 7) + Modl(10) * Proj(11) + Modl(11) * Proj(15);

      Clip(12) := Modl(12) * Proj( 0) + Modl(13) * Proj( 4) + Modl(14) * Proj( 8) + Modl(15) * Proj(12);
      Clip(13) := Modl(12) * Proj( 1) + Modl(13) * Proj( 5) + Modl(14) * Proj( 9) + Modl(15) * Proj(13);
      Clip(14) := Modl(12) * Proj( 2) + Modl(13) * Proj( 6) + Modl(14) * Proj(10) + Modl(15) * Proj(14);
      Clip(15) := Modl(12) * Proj( 3) + Modl(13) * Proj( 7) + Modl(14) * Proj(11) + Modl(15) * Proj(15);

      -- Extract the RIGHT plane
      the_Planes (Right)(0) := GL.Double (Clip( 3) - Clip( 0));
      the_Planes (Right)(1) := GL.Double (Clip( 7) - Clip( 4));
      the_Planes (Right)(2) := GL.Double (Clip(11) - Clip( 8));
      the_Planes (Right)(3) := GL.Double (Clip(15) - Clip(12));

      -- Extract the LEFT plane
      the_Planes (Left)(0) := GL.Double (Clip( 3) + Clip( 0));
      the_Planes (Left)(1) := GL.Double (Clip( 7) + Clip( 4));
      the_Planes (Left)(2) := GL.Double (Clip(11) + Clip( 8));
      the_Planes (Left)(3) := GL.Double (Clip(15) + Clip(12));

      -- Extract the LOW plane
      the_Planes (Low)(0) := GL.Double (Clip( 3) + Clip( 1));
      the_Planes (Low)(1) := GL.Double (Clip( 7) + Clip( 5));
      the_Planes (Low)(2) := GL.Double (Clip(11) + Clip( 9));
      the_Planes (Low)(3) := GL.Double (Clip(15) + Clip(13));

      -- Extract the HIGH plane
      the_Planes (High)(0) := GL.Double (Clip( 3) - Clip( 1));
      the_Planes (High)(1) := GL.Double (Clip( 7) - Clip( 5));
      the_Planes (High)(2) := GL.Double (Clip(11) - Clip( 9));
      the_Planes (High)(3) := GL.Double (Clip(15) - Clip(13));

      -- Extract the FAR plane
      the_Planes (Far)(0) := GL.Double (Clip( 3) - Clip( 2));
      the_Planes (Far)(1) := GL.Double (Clip( 7) - Clip( 6));
      the_Planes (Far)(2) := GL.Double (Clip(11) - Clip(10));
      the_Planes (Far)(3) := GL.Double (Clip(15) - Clip(14));

      -- Extract the NEAR plane
      the_Planes (Near)(0) := GL.Double (Clip( 3) + Clip( 2));
      the_Planes (Near)(1) := GL.Double (Clip( 7) + Clip( 6));
      the_Planes (Near)(2) := GL.Double (Clip(11) + Clip(10));
      the_Planes (Near)(3) := GL.Double (Clip(15) + Clip(14));

      normalise (the_Planes);
      return the_Planes;
   end;

end GL.Frustums;
