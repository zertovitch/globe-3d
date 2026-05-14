with
     GLOBE_3D.Skinned_Visuals,
     GLOBE_3D.Visuals_Rendering,

     GLUT.Windows,
     GL;

use
    GLOBE_3D,
    GLUT.Windows;

procedure launch_multi_Window
is
   use type GL.Double;

   Viewer_1   : GLUT.Windows.Window;
   Viewer_2   : GLUT.Windows.Window;
   Viewer_3   : GLUT.Windows.Window;

   the_Object : constant GLOBE_3D.Skinned_Visuals.p_Skinned_Visual := null;

begin
   Set_Global_Data_Name ("../g3demo_global_resources.zip");
   Set_Level_Data_Name  ("../g3demo_level_resources.zip");

   GLUT.Windows.Initialize;

   Viewer_1.Set_Renderer (GLOBE_3D.Visuals_Rendering.Render'Access);
   Viewer_2.Set_Renderer (GLOBE_3D.Visuals_Rendering.Render'Access);
   Viewer_3.Set_Renderer (GLOBE_3D.Visuals_Rendering.Render'Access);

   Define (Viewer_1);
   Define (Viewer_2);
   Define (Viewer_3);

   --  Box.Create (the_Object);  --  tbd: box as "skinned visual"

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
