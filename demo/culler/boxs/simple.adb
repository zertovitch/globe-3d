
with GLOBE_3D;                              use GLOBE_3D;
with globe_3d.Culler.impostoring_frustum;   use globe_3d.Culler.impostoring_frustum;

with GLUT.Windows;                          use GLUT.Windows;
with GLUT.Devices;                          use GLUT.Devices;
with gl;

with Box;

with ada.text_io;                           use ada.text_io;



procedure Simple
is
   use type Real;
   package g3d renames GLOBE_3D;

   Viewer : aliased GLUT.windows.Window;
   Culler :         globe_3d.culler.impostoring_frustum.Culler;

   the_Object : g3d.p_Object_3D;
begin
   g3d.Set_level_data_name  ("../../G3Demo_Level_Resources.zip");
   g3d.Set_global_data_name ("../../G3Demo_Global_Resources.zip");

   GLUT.Windows.initialize;


   define (Viewer);
   Culler.Viewer_is (Viewer'unchecked_access);    -- tell culler where to send culled visuals.


   box.create (the_Object);

   add (Culler,  the_Object.all'access);


   for Each_x in 1 .. 50 loop
      for Each_z in 1 .. 50 loop
         box.create (the_Object);
         the_Object.Centre := (-50.0 + Real (Each_x) * 2.0,  1.0,  -50.0 + Real (Each_z) * 2.0);
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
