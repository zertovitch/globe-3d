
with GLOBE_3D;                              use GLOBE_3D;
with globe_3d.Culler.impostoring_frustum;   use globe_3d.Culler.impostoring_frustum;

with GLUT.Windows;                          use GLUT.Windows;
with GLUT.Devices;                          use GLUT.Devices;
with gl;

with Box;
with X29;

with ada.text_io;                           use ada.text_io;



procedure Simple
is
   use type Real;
   package g3d renames GLOBE_3D;

   Viewer : aliased GLUT.windows.Window;
   Culler :         globe_3d.culler.impostoring_frustum.Culler;

   the_Object : g3d.p_Object_3D;
begin

   declare
      Usage_1 : String :=  "These keys allow aspects of the culler to be turned on and off:";
      Usage_2 : String :=  "   'v' : enable/disable 'vanishing point' culling (only applies when objects";
      Usage_3 : String :=  "         are *very* distant ... ie past the 'vanishing point').";
      Usage_4 : String :=  "   'i' : enable/disable use of 'impostor's (most noticeable when entire grid is visible)";
      Usage_5 : String :=  "   'b' : enable/disable 'frustum culling' (most noticeable when in the middle";
      Usage_6 : String :=  "         of the grid ... ie at the starting position).";
   begin
      new_Line;
      put_Line (Usage_1);
      new_Line;
      put_Line (Usage_2);
      put_Line (Usage_3);
      new_Line;
      put_Line (Usage_4);
      new_Line;
      put_Line (Usage_5);
      put_Line (Usage_6);
      new_Line;
   end;


   g3d.Set_level_data_name  ("../../G3Demo_Level_Resources.zip");
   g3d.Set_global_data_name ("../../G3Demo_Global_Resources.zip");

   GLUT.Windows.initialize;


   define (Viewer);
   Culler.Viewer_is (Viewer'unchecked_access);    -- tell culler where to send culled visuals.


   for Each_x in 1 .. 30 loop
      for Each_z in 1 .. 30 loop
         -- Box.create (the_Object);
         X29.Create(object => the_Object,   scale  => 1.0,
                                            centre => (0.0,0.0,-17.0));
--           the_Object.Centre := (-10.0 + Real (Each_x) * 2.0,  1.0,  -10.0 + Real (Each_z) * 2.0);
         the_Object.Centre := (-600.0 + Real (Each_x) * 30.0,  -20.0,  -600.0 + Real (Each_z) * 30.0);
         add (Culler,  the_Object.all'access);
      end loop;
   end loop;


   declare
      default_vanish_point_size_Min : Real := Culler.vanish_point_size_Min;
      default_impostor_size_Min     : Real := Culler.impostor_size_Min;

   begin

      while not Viewer.is_closed loop
         GLUT.mainLoopEvent;

         if strike_Once ('V', viewer.Keyboard) then
            if Culler.vanish_point_size_Min = default_vanish_point_size_Min then
               Culler.vanish_point_size_Min_is (0.0);
            else
               Culler.vanish_point_size_Min_is (default_vanish_point_size_Min);
            end if;

         elsif strike_Once ('B', viewer.Keyboard) then
            if Culler.frustum_culling_Enabled then
               Culler.frustum_culling_Enabled_is (False);
            else
               Culler.frustum_culling_Enabled_is (True);
            end if;

         elsif strike_Once ('I', viewer.Keyboard) then
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
   put_Line ("Done.");
end Simple;
