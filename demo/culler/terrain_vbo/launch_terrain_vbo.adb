with
     GLOBE_3D.Culler.Impostoring_frustum,
     GLOBE_3D.Sprite,
     GLOBE_3D.Visuals_rendering,
     Terrain.VBO,
     GL.Buffer,
     GL.Extended,
     GLUT.Windows,
     Ada.Text_IO;
use
    GLOBE_3D.Culler.Impostoring_frustum,
    GLOBE_3D.Sprite,
    Terrain.VBO,
    GLUT.Windows,
    Ada.Text_IO;
with GL.Skins;


procedure launch_Terrain_vbo
is
   package g3d renames GLOBE_3D;

   Viewer     : aliased GLUT.Windows.Window;
   Culler     :         GLOBE_3D.Culler.Impostoring_frustum.Culler;

   procedure VBO_Callback
   is
   begin
      GL.Extended.BindBuffer (GL.Extended.ARRAY_BUFFER, 0);
   end VBO_Callback;

begin
   GL.Skins.Disable_VBO_callback := VBO_Callback'Unrestricted_Access;

   g3d.Set_global_data_name ("../../G3Demo_Global_Resources.zip");
   g3d.Set_level_data_name  ("../../G3Demo_Level_Resources.zip");

   GLUT.Windows.initialize;
   Viewer.Set_renderer(GLOBE_3D.Visuals_rendering.Render'Access);

   -- Setup the viewing window and inform the culler.
   --
   define (Viewer);
   Viewer.Camera.clipper.eye_position := (0.0, 200.0, 0.0);
   Culler.Viewer_is (Viewer'Unchecked_Access);                   -- Tell culler where to send culled visuals.

   -- Add the terrain.
   --
   declare
      terrain_Grid : constant GLOBE_3D.Sprite.p_sprite_Grid
        := Terrain.VBO.Create (tga_Heights   => "irin-heightmap-512.tga",
                               texture_Image => "irin-texturemap-512.bmp",
                               flip_Vertical => True,
                               tile_Width    => 32,
                               tile_Depth    => 32,
                               base_Centre   => (0.0, 0.0, 0.0),
                               Scale         => (8.0, 2.0, 8.0));
   begin
      for Row in terrain_Grid'Range (1) loop
         for Col in terrain_Grid'Range (2) loop
            add (Culler,  terrain_Grid (Row, Col).all'Access);
         end loop;
      end loop;
   end;

   -- Main loop.
   --
   while not Viewer.is_Closed loop
      GLUT.MainLoopEvent;
      evolve (Culler,  By => 0.02);
   end loop;

   destroy (Viewer);
   Put_Line ("Done.");

exception
   when GL.Buffer.no_platform_Support =>
      Put_Line ("OpenGL support on this platform does not appear to include 'vertex buffer objects'.");
      Put_Line ("... unable to run demo.");
end launch_Terrain_vbo;
