with GLOBE_3D;     use GLOBE_3D;
with GLOBE_3D.Textures;

with GLUT.Windows; use GLUT.Windows;
with gl;

with Box;

with ada.text_io; use ada.text_io;



procedure Simple
is
   use GL;
   package g3d renames GLOBE_3D;

   Viewer_1   : GLUT.windows.Window;
   Viewer_2   : GLUT.windows.Window;
   Viewer_3   : GLUT.windows.Window;

   the_Object : g3d.p_Object_3D;
begin
   g3d.Set_level_data_name  ("../G3Demo_Global_Resources.zip");
   g3d.Set_global_data_name ("../G3Demo_Level_Resources.zip");

   GLUT.Windows.initialize;

   define (Viewer_1);
   define (Viewer_2);
   define (Viewer_3);

   G3D.Textures.Reserve_Textures(6);

   box.create (the_Object);

   the_Object.Centre := (0.0, 2.0, -5.0);

   add (Viewer_1,  the_Object.all'access);
   add (Viewer_2,  the_Object.all'access);
   add (Viewer_3,  the_Object.all'access);

   loop
      GLUT.mainLoopEvent;

      exit when     Viewer_1.is_Closed
                and Viewer_2.is_Closed
                and Viewer_3.is_Closed;

      freshen (Viewer_1,  time_step => 0.02);
      freshen (Viewer_2,  time_step => 0.02);
      freshen (Viewer_3,  time_step => 0.02);

   end loop;

   destroy (Viewer_1);
   destroy (Viewer_2);
   destroy (Viewer_3);

   put_Line ("Done");
end Simple;
