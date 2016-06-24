--  This was moved from the main GLOBE_3D package (23-June-2016).
--  Reason: the codes depends on GL extensions (VBOs), which is cumbersome on some platforms.
--  Example: the version of gprbuild, GNAT GPL 2016 in Windows fails at linking stage
--  but the deprecated gnatmake succeeds. Plus, all the mess with GLEE, GLEW, etc.

with GLOBE_3D.Math;

with GL.Errors, GL.Math, GL.Skins;
with GL.Skinned_Geometry;

with System.Storage_Elements;
with Ada.Containers.Generic_Array_Sort;

package body GLOBE_3D.Visuals_rendering is

  package G3DM renames GLOBE_3D.Math;

   ----------------------------------------
   -- tbd: has been moved (for the moment) external to 'render' for performance, but this makes package task unsafe !
   --
   --
      type Visual_Geometry is
         record
            Visual   : p_Visual;
            Geometry : GL.Skinned_Geometry.Skinned_Geometry;
         end record;
      pragma Convention (C, Visual_Geometry);  -- using convention pragma to disable default initialization (for performance)

      type Visual_Geometries is array (Positive range <>) of Visual_Geometry;
      pragma Convention (C, Visual_Geometries);  -- using convention pragma to disable default initialization (for performance)

   all_Geometries     : Visual_Geometries (1 .. 80_000);   pragma Convention (C, all_Geometries);  -- tbd: this is slow !
   --
   --------------------------------------

   procedure Render (the_Visuals : in Visual_array;   the_Camera : in Camera'Class)
   is
      use GL, REF, G3DM;

      all_Transparents  : GLOBE_3D.Visual_array (1 .. 10_000);
      transparent_Count : Natural                           := 0;

      geometry_Count    : Natural                       := 0;   -- for 'all_Geometries' array.

      current_Skin      : GL.Skins.p_Skin;

   begin
      -- prepare openGL to display visuals.
      --
      Clear    (COLOR_BUFFER_BIT or DEPTH_BUFFER_BIT);
      Enable   (DEPTH_TEST);

      Enable   (LIGHTING);                               -- enable lighting for G3D.Display in 'separate Visuals' (obsolete).
      Enable   (CULL_FACE);
      CullFace (BACK);

      MatrixMode    (MODELVIEW);
      Set_GL_Matrix (the_Camera.world_rotation);
      Translate     (-the_Camera.clipper.eye_position (0),  -the_Camera.clipper.eye_position (1),  -the_Camera.clipper.eye_position (2));

      PushMatrix;

      -- separate Visuals
      --
      for Each in the_Visuals'Range loop
         declare
            use GL.Skinned_Geometry;
            the_Visual        : Visual'Class                           renames the_Visuals (Each).all;
            visual_geometries : GL.Skinned_Geometry.Skinned_Geometries renames the_Visual.Skinned_Geometries;
--                   GL.Skinned_Geometry.null_skinned_geometries;
                 -- GdM 23-Jun-2016: !! root function Skinned_Geometries (the_Visual) returned always null_skinned_geometries;
         begin
            if Is_Transparent (the_Visual) then
               transparent_Count                    := transparent_Count + 1;
               all_Transparents (transparent_Count) := the_Visual'Access;
            else
               for Each in visual_geometries'Range loop
                  geometry_Count                          := geometry_Count + 1;
                  all_Geometries (geometry_Count).Visual   := the_Visual'Access;
                  all_Geometries (geometry_Count).Geometry := visual_geometries (Each);
               end loop;

               Display (the_Visuals (Each).all,  the_Camera.clipper);
            end if;
         end;
      end loop;

      GL.Errors.Log;

      -- display all opaque geometries, sorted by gl geometry primitive kind and skin.
      --
      declare
         function "<" (L, R : in Visual_Geometry) return Boolean
         is
            use GL.Geometry, System.Storage_Elements;
         begin
            if primitive_Id (L.Geometry.Geometry.all)  =  primitive_Id (R.Geometry.Geometry.all) then   -- tbd: find better naming scheme to avoid '.Geometry.Geometry.'
               return To_Integer (L.Geometry.Skin.all'Address)  <  To_Integer (R.Geometry.Skin.all'Address); -- tbd: check this is safe/portable
               -- GdM: aaargh! remove that !!
            elsif primitive_Id (L.Geometry.Geometry.all)  <  primitive_Id (R.Geometry.Geometry.all) then
               return True;

            else -- L.Geometry.primitive_Id > R.Geometry.primitive_Id
               return False;
            end if;
         end "<";

         procedure Sort is new Ada.Containers.Generic_Array_Sort (Positive,
                                                                  Visual_Geometry,
                                                                  Visual_Geometries);
         use GL.Skins, GL.Geometry, GL.Skinned_Geometry;

         current_Visual : p_Visual;

      begin
         if geometry_Count > 1 then
            Sort (all_Geometries (1 .. geometry_Count));
         end if;

         GL.PushMatrix;

         for Each in 1 .. geometry_Count loop

            if all_Geometries (Each).Geometry.Skin /= current_Skin then
               current_Skin := all_Geometries (Each).Geometry.Skin;
               enable (current_Skin.all);
               GL.Errors.Log;
            end if;

            if all_Geometries (Each).Geometry.Veneer /= null then
               enable (all_Geometries (Each).Geometry.Veneer.all);
               GL.Errors.Log;
            end if;

            if all_Geometries (Each).Visual = current_Visual then
               draw (all_Geometries (Each).Geometry.Geometry.all);
               GL.Errors.Log;
            else
               GL.PopMatrix;
               GL.PushMatrix;
               GL.Translate       (all_Geometries (Each).Visual.centre);
               Multiply_GL_Matrix (all_Geometries (Each).Visual.rotation);

               draw (all_Geometries (Each).Geometry.Geometry.all);
               GL.Errors.Log;

               current_Visual := all_Geometries (Each).Visual;
            end if;

         end loop;

         GL.PopMatrix;
      end;

      GL.Errors.Log;

      -- display all transparent visuals, sorted from far to near.
      --
      declare
         function "<" (L, R : in GLOBE_3D.p_Visual) return Boolean -- tbd : ugh move expensive calcs outside
         is
         begin
            return L.centre_camera_space (2) < R.centre_camera_space (2);  -- nb: in camera space, negative Z is forward, so use '<'.
         end "<";

         --procedure sort is new Ada.Containers.Generic_Array_Sort (Positive,
         procedure sort is new Ada.Containers.Generic_Array_Sort (Positive,
                                                                  GLOBE_3D.p_Visual,
                                                                  GLOBE_3D.Visual_array);
         use GL.Math;
      begin
         for Each in 1 .. transparent_Count loop  -- pre-calculate each visuals Centre in camera space.
            all_Transparents (Each).centre_camera_space :=   the_Camera.world_rotation
                                                           * (all_Transparents (Each).centre - the_Camera.clipper.eye_position);
         end loop;

         if transparent_Count > 1 then
            sort (all_Transparents (1 .. transparent_Count));
         end if;

         GL.DepthMask (GL_FALSE);  -- make depth buffer read-only, for correct transparency

         Enable    (LIGHTING);   -- ensure lighting is enabled for G3D.Display of transparents (obsolete).
         Enable    (BLEND);
         BlendFunc (sfactor => ONE,
                    dfactor => ONE_MINUS_SRC_ALPHA);

         for Each in 1 .. transparent_Count loop
            declare
               the_Visual        : Visual'Class                          renames all_Transparents (Each).all;
               visual_Geometries : constant GL.Skinned_Geometry.Skinned_Geometries      :=
                 GL.Skinned_Geometry.null_skinned_geometries;  --  Skinned_Geometries (the_Visual)
                 -- GdM 23-Jun-2016: !! root function Skinned_Geometries (the_Visual) returned always null_skinned_geometries;
                 -- tbd: apply ogl state sorting here ?
            begin
               Display (the_Visual,  the_Camera.clipper);
               GL.Errors.Log;

               for Each in visual_Geometries'Range loop
                  declare
                     use GL.Skins, GL.Geometry;
                     the_Geometry : GL.Skinned_Geometry.Skinned_Geometry renames visual_Geometries (Each);
                  begin

                     if the_Geometry.Skin /= current_Skin then
                        current_Skin := the_Geometry.Skin;
                        enable (current_Skin.all);
                        GL.Errors.Log;
                     end if;

                     if the_Geometry.Veneer /= null then
                        enable (the_Geometry.Veneer.all);
                        GL.Errors.Log;
                     end if;

                     GL.PushMatrix;

                     GL.Translate       (the_Visual.centre);
                     Multiply_GL_Matrix (the_Visual.rotation);

                     draw (the_Geometry.Geometry.all);
                     GL.Errors.Log;

                     GL.PopMatrix;
                  end;
               end loop;

            end;
         end loop;

         GL.DepthMask (GL_TRUE);
      end;

      PopMatrix;

      GL.Errors.Log;      -- tbd: for debug only
   end Render;

end GLOBE_3D.Visuals_rendering;
