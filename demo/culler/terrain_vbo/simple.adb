
with globe_3d.Culler.impostoring_frustum;   use globe_3d.Culler.impostoring_frustum;
with globe_3d.Sprite;                       use globe_3d.Sprite;
with Terrain.vbo;                           use Terrain.vbo;

with GL.Buffer;
with GLUT.Windows;                          use GLUT.Windows;

with ada.Text_IO;                           use ada.Text_IO;



procedure Simple
is
   package g3d renames GLOBE_3D;

   Viewer     : aliased GLUT.windows.Window;
   Culler     :         globe_3d.culler.impostoring_frustum.Culler;

begin
   g3d.Set_global_data_name ("../../G3Demo_Global_Resources.zip");
   g3d.Set_level_data_name  ("../../G3Demo_Level_Resources.zip");

   GLUT.Windows.initialize;


   -- setup the viewing window and inform the culler.
   --
   define (Viewer);
   Viewer.Camera.clipper.eye_Position := (0.0, 200.0, 0.0);
   Culler.Viewer_is (Viewer'unchecked_access);                   -- tell culler where to send culled visuals.


   -- add the terrain
   --
   declare
      terrain_Grid : GLOBE_3D.sprite.p_sprite_Grid := Terrain.vbo.create (tga_heights   => "irin-heightmap-512.tga",
                                                                          texture_image => "irin-texturemap-512.bmp",
                                                                          tile_width    => 32,
                                                                          tile_depth    => 32,
                                                                          base_centre   => (0.0, 0.0, 0.0),
                                                                          scale         => (8.0, 2.0, 8.0));
   begin
      for Row in terrain_Grid'range (1) loop
         for Col in terrain_Grid'range (2) loop
            add (Culler,  terrain_Grid (Row, Col).all'access);
         end loop;
      end loop;
   end;


   -- main loop
   --
   while not Viewer.is_closed loop
      GLUT.mainLoopEvent;
      evolve (Culler,  by => 0.02);
   end loop;

   destroy (Viewer);
   put_Line ("Done.");

exception
   when gl.buffer.no_platform_Support =>
      put_Line ("OpenGL support on this platform does not appear to include 'vertex buffer objects'.");
      put_Line ("... unable to run demo.");
end Simple;
