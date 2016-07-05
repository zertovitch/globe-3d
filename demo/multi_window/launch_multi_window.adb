with
     GLOBE_3D,
     GLOBE_3D.Visuals_rendering,

     GLUT.Windows,
     GL,

     Box;

use
    GLOBE_3D,
    GLUT.Windows;

procedure launch_multi_Window
is
   use type GL.Double;

   Viewer_1   : GLUT.Windows.Window;
   Viewer_2   : GLUT.Windows.Window;
   Viewer_3   : GLUT.Windows.Window;

   the_Object : p_Object_3D;

begin
   Set_level_data_name  ("../G3Demo_Global_Resources.zip");
   Set_global_data_name ("../G3Demo_Level_Resources.zip");

   GLUT.Windows.initialize;

   Viewer_1.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);
   Viewer_2.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);
   Viewer_3.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);

   define (Viewer_1);
   define (Viewer_2);
   define (Viewer_3);

   Box.create (the_Object);

   the_Object.Centre := (0.0, 2.0, -5.0);

   add (Viewer_1,  the_Object.all'Access);
   add (Viewer_2,  the_Object.all'Access);
   add (Viewer_3,  the_Object.all'Access);

   loop
      GLUT.MainLoopEvent;

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
end launch_multi_Window;
