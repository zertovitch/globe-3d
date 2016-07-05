with
     GLOBE_3D.Sprite,
     GLOBE_3D.Culler.impostoring_frustum,
     GLOBE_3D.Visuals_rendering,

     GLUT.Windows,
     GLUT.Devices,

     GL.Extended,
     GL.Skins,

     X29_vbo,

     Ada.Text_IO;

use
    GLOBE_3D,
    GLOBE_3D.Sprite,
    GLOBE_3D.Culler.impostoring_frustum,

    GLUT.Windows,
    GLUT.Devices,

    Ada.Text_IO;

procedure launch_Armada
is
   use type Real;
   package g3d renames GLOBE_3D;

   Viewer : aliased GLUT.Windows.Window;
   Culler :         GLOBE_3D.Culler.impostoring_frustum.Culler;

   procedure VBO_Callback
   is
   begin
      GL.Extended.BindBuffer (GL.Extended.ARRAY_BUFFER, 0);
   end VBO_Callback;

begin
   GL.Skins.Disable_VBO_callback := VBO_Callback'Unrestricted_Access;

   declare
      Usage_1 : constant String :=  "These keys allow aspects of the culler to be turned on and off:";
      Usage_2 : constant String :=  "   'v' : enable/disable 'vanishing point' culling (only applies when objects";
      Usage_3 : constant String :=  "         are *very* distant ... ie past the 'vanishing point').";
      Usage_4 : constant String :=  "   'i' : enable/disable use of 'impostor's (most noticeable when entire grid is visible)";
      Usage_5 : constant String :=  "   'b' : enable/disable 'frustum culling' (most noticeable when in the middle";
      Usage_6 : constant String :=  "         of the grid ... ie at the starting position).";
   begin
      New_Line;
      Put_Line (Usage_1);
      New_Line;
      Put_Line (Usage_2);
      Put_Line (Usage_3);
      New_Line;
      Put_Line (Usage_4);
      New_Line;
      Put_Line (Usage_5);
      Put_Line (Usage_6);
      New_Line;
   end;

   g3d.Set_level_data_name  ("../../G3Demo_Level_Resources.zip");
   g3d.Set_global_data_name ("../../G3Demo_Global_Resources.zip");

   GLUT.Windows.initialize;
   Viewer.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);

   define (Viewer);
   Culler.Viewer_is (Viewer'Unchecked_Access);    -- Tell culler where to send culled visuals.

   -- Add X29_vbo's.
   --
   declare
      an_X29 : g3d.Sprite.p_Sprite;
   begin
      for x in 1 .. 50 loop
         for z in 1 .. 50 loop
            X29_vbo.Create (an_X29,   scale  => 1.0,
                                      centre => (0.0, 0.0, -17.0));

            an_X29.centre := (-600.0 + Real (x) * 30.0,
                              -20.0,
                              -600.0 + Real (z) * 30.0);

            add (Culler, an_X29.all'Access);
         end loop;
      end loop;
   end;

   declare
      default_vanish_point_size_Min : constant Real := Culler.vanish_point_size_Min;
      default_impostor_size_Min     : constant Real := Culler.impostor_size_Min;
   begin
      while not Viewer.is_Closed
      loop
         GLUT.MainLoopEvent;

         if strike_Once ('V', Viewer.Keyboard)
         then
            if Culler.vanish_point_size_Min = default_vanish_point_size_Min then
               Culler.vanish_point_size_Min_is (0.0);
            else
               Culler.vanish_point_size_Min_is (default_vanish_point_size_Min);
            end if;

         elsif strike_Once ('B', Viewer.Keyboard)
         then
            if Culler.frustum_culling_Enabled then
               Culler.frustum_culling_Enabled_is (False);
            else
               Culler.frustum_culling_Enabled_is (True);
            end if;

         elsif strike_Once ('I', Viewer.Keyboard)
         then
            if Culler.impostor_size_Min = default_impostor_size_Min then
               Culler.impostor_size_Min_is (0.0);
            else
               Culler.impostor_size_Min_is (default_impostor_size_Min);
            end if;
         end if;

         evolve (Culler,  by => 0.02);
      end loop;
   end;

   destroy (Viewer);
   Put_Line ("Done.");
end launch_Armada;
