
with GLOBE_3D;                              use GLOBE_3D;
with globe_3d.Culler.impostoring_frustum;   use globe_3d.Culler.impostoring_frustum;

with GLUT.Windows;                          use GLUT.Windows;
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


   while not Viewer.is_closed loop
      GLUT.mainLoopEvent;

      evolve (Culler,  by => 0.02);
   end loop;

   destroy (Viewer);
   put_Line ("Done.");
end Simple;
