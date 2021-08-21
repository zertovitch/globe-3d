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
   Set_global_data_name ("../g3demo_global_resources.zip");
   Set_level_data_name  ("../g3demo_level_resources.zip");

   GLUT.Windows.Initialize;

   Viewer_1.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);
   Viewer_2.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);
   Viewer_3.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);

   Define (Viewer_1);
   Define (Viewer_2);
   Define (Viewer_3);

   Box.Create (the_Object);

   the_Object.centre := (0.0, 2.0, -5.0);

   Add (Viewer_1,  the_Object.all'Access);
   Add (Viewer_2,  the_Object.all'Access);
   Add (Viewer_3,  the_Object.all'Access);

   loop
      GLUT.MainLoopEvent;

      exit when     Viewer_1.Is_Closed
                and Viewer_2.Is_Closed
                and Viewer_3.Is_Closed;

      Freshen (Viewer_1,  Time_Step => 0.02);
      Freshen (Viewer_2,  Time_Step => 0.02);
      Freshen (Viewer_3,  Time_Step => 0.02);
   end loop;

   Destroy (Viewer_1);
   Destroy (Viewer_2);
   Destroy (Viewer_3);
end launch_multi_Window;
